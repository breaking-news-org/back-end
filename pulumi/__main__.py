"""
Creating a Kubernetes Deployment
"""
import pulumi
from pulumi_kubernetes.batch.v1 import Job, JobSpecArgs
from pulumi_kubernetes.apps.v1 import Deployment, DeploymentSpecArgs
import pulumi_kubernetes.core.v1 as k8s
from pulumi_kubernetes.meta.v1 import ObjectMetaArgs, LabelSelectorArgs
from pulumi_kubernetes.provider import Provider
from pulumi import ResourceOptions
from copy import deepcopy
import pulumi_kubernetes as kubernetes
import yaml


def mk_full_name(environment, name):
    return f"{environment}-{name}"


def mk_postgres(postgres_config, environment, provider=None):
    name = postgres_config["name"]
    full_name = mk_full_name(environment=environment, name=name)
    db_config = postgres_config["db"]
    container_config = postgres_config["container"]
    persistent_volume_config = postgres_config["persistentVolume"]
    service_config = postgres_config["service"]
    labels = {"environment": environment}

    config_map = (
        lambda name_=f"{full_name}-configmap": k8s.ConfigMap(
            resource_name=name_,
            api_version="v1",
            kind="ConfigMap",
            metadata=ObjectMetaArgs(
                name=name_,
                labels=labels,
            ),
            data=postgres_config["configMap"]["data"],
            opts=ResourceOptions(provider=provider),
        )
    )()

    persistent_volume = (
        lambda name_=f"{full_name}-persistent-volume": k8s.PersistentVolume(
            resource_name=name_,
            kind="PersistentVolume",
            api_version="v1",
            metadata=ObjectMetaArgs(
                name=name_,
                labels=labels,
            ),
            spec=k8s.PersistentVolumeSpecArgs(
                storage_class_name="manual",
                capacity={
                    "storage": "5Gi",
                },
                access_modes=["ReadWriteMany"],
                host_path=k8s.HostPathVolumeSourceArgs(
                    path=persistent_volume_config["hostPath"],
                ),
            ),
            opts=ResourceOptions(provider=provider, delete_before_replace=True),
        )
    )()

    persistent_volume_claim = (
        lambda name_=f"{full_name}-persistent-volume-claim": k8s.PersistentVolumeClaim(
            resource_name=name_,
            kind="PersistentVolumeClaim",
            api_version="v1",
            metadata=ObjectMetaArgs(
                name=name_,
                labels=labels,
            ),
            spec=k8s.PersistentVolumeClaimSpecArgs(
                storage_class_name="manual",
                access_modes=["ReadWriteMany"],
                resources=k8s.ResourceRequirementsArgs(
                    requests={
                        "storage": "5Gi",
                    },
                ),
            ),
            opts=ResourceOptions(provider=provider, delete_before_replace=True),
        )
    )()

    db_port = f"db-port"

    deployment = (
        lambda name_=f"{full_name}-deployment": Deployment(
            resource_name=name_,
            api_version="apps/v1",
            kind="Deployment",
            metadata=ObjectMetaArgs(name=name_, labels=labels),
            spec=DeploymentSpecArgs(
                replicas=1,
                selector=LabelSelectorArgs(
                    match_labels=labels,
                ),
                template=k8s.PodTemplateSpecArgs(
                    metadata=ObjectMetaArgs(
                        labels=labels,
                    ),
                    spec=(
                        lambda volume_name=f"{name_}-container-volume": k8s.PodSpecArgs(
                            containers=[
                                k8s.ContainerArgs(
                                    name=f"{name_}-container",
                                    image=container_config["image"],
                                    image_pull_policy="IfNotPresent",
                                    ports=[
                                        k8s.ContainerPortArgs(
                                            container_port=db_config["port"],
                                            name=db_port,
                                        )
                                    ],
                                    env_from=[
                                        k8s.EnvFromSourceArgs(
                                            config_map_ref=k8s.ConfigMapEnvSourceArgs(
                                                name=config_map.metadata.name
                                            )
                                        )
                                    ],
                                    volume_mounts=[
                                        k8s.VolumeMountArgs(
                                            name=volume_name,
                                            mount_path=container_config["volumeMounts"][
                                                "mountPath"
                                            ],
                                        )
                                    ],
                                    resources=k8s.ResourceRequirementsArgs(
                                        requests={
                                            "memory": "64Mi",
                                            "cpu": "250m",
                                        },
                                        limits={
                                            "memory": "128Mi",
                                            "cpu": "500m",
                                        },
                                    ),
                                )
                            ],
                            volumes=[
                                k8s.VolumeArgs(
                                    name=volume_name,
                                    persistent_volume_claim=k8s.PersistentVolumeClaimVolumeSourceArgs(
                                        claim_name=persistent_volume_claim.metadata.name,
                                    ),
                                )
                            ],
                        )
                    )(),
                ),
            ),
            opts=ResourceOptions(provider=provider, delete_before_replace=True),
        )
    )()

    service_name = f"{full_name}-service"
    port_name = f"{service_name}-port"
    service = k8s.Service(
        resource_name=service_name,
        api_version="v1",
        kind="Service",
        metadata=ObjectMetaArgs(
            name=service_name,
            labels=labels,
        ),
        spec=k8s.ServiceSpecArgs(
            type=service_config["type"],
            ports=[
                k8s.ServicePortArgs(
                    port=service_config["port"],
                    # TODO ?
                    name=port_name,
                    target_port=db_port,
                    node_port=service_config["nodePort"],
                )
            ],
            # TODO fix
            selector=labels,
        ),
        opts=ResourceOptions(provider=provider, delete_before_replace=True),
    )

    host = service_name
    port = service_config["port"]
    return host, port


def mk_back(back_config, environment, postgres_host, postgres_port, provider=None):
    name = back_config["name"]
    full_name = mk_full_name(environment=environment, name=name)
    deployment_config = back_config["deployment"]
    container_config = deployment_config["container"]
    service_config = back_config["service"]
    labels = {
        "environment": environment,
    }

    app_config = container_config["config"]["app"]
    jwk_config = container_config["config"]["jwk"]

    app_config["file"]["db"]["host"] = postgres_host
    app_config["file"]["db"]["port"] = postgres_port

    config_map = (
        lambda name_=f"{full_name}-configmap": k8s.ConfigMap(
            resource_name=name_,
            api_version="v1",
            kind="ConfigMap",
            metadata=ObjectMetaArgs(
                name=name_,
                labels=labels,
            ),
            data={
                app_config["filePath"]: str(yaml.dump(app_config["file"])),
                jwk_config["filePath"]: str(yaml.dump(jwk_config["file"])),
            },
            opts=ResourceOptions(provider=provider, delete_before_replace=True),
        )
    )()

    container_port = app_config["file"]["web"]["port"]
    container_port_name = f"{name}-port"

    deployment = (
        lambda name_=f"{full_name}-deployment": Deployment(
            resource_name=name_,
            api_version="apps/v1",
            kind="Deployment",
            metadata=ObjectMetaArgs(name=name_, labels=labels),
            spec=DeploymentSpecArgs(
                replicas=1,
                selector=LabelSelectorArgs(
                    match_labels=labels,
                ),
                template=k8s.PodTemplateSpecArgs(
                    metadata=ObjectMetaArgs(
                        labels=labels,
                    ),
                    spec=(
                        lambda app_volume=f"{name_}-app-config-volume", jwk_volume=f"{name_}-jwk-volume", configMountPath=container_config[
                            "config"
                        ][
                            "mountPath"
                        ]: k8s.PodSpecArgs(
                            containers=[
                                (
                                    lambda container_name=container_config[
                                        "name"
                                    ], docker_hub_image=container_config[
                                        "dockerHubImage"
                                    ]: k8s.ContainerArgs(
                                        name=container_name,
                                        image=docker_hub_image,
                                        image_pull_policy="Always",
                                        ports=[
                                            k8s.ContainerPortArgs(
                                                container_port=container_port,
                                                name=container_port_name,
                                            )
                                        ],
                                        volume_mounts=[
                                            k8s.VolumeMountArgs(
                                                mount_path=configMountPath,
                                                name=app_volume,
                                            ),
                                        ],
                                        env=[
                                            k8s.EnvVarArgs(
                                                name=app_config["varName"],
                                                value=f"{configMountPath}/{app_config['filePath']}",
                                            ),
                                            k8s.EnvVarArgs(
                                                name=jwk_config["varName"],
                                                value=f"{configMountPath}/{jwk_config['filePath']}",
                                            ),
                                        ],
                                        resources=k8s.ResourceRequirementsArgs(
                                            requests={
                                                "memory": "64Mi",
                                                "cpu": "250m",
                                            },
                                            limits={
                                                "memory": "128Mi",
                                                "cpu": "500m",
                                            },
                                        ),
                                    )
                                )()
                            ],
                            volumes=[
                                k8s.VolumeArgs(
                                    name=app_volume,
                                    config_map=k8s.ConfigMapVolumeSourceArgs(
                                        name=config_map.metadata.name
                                    ),
                                )
                            ],
                        )
                    )(),
                ),
            ),
            opts=ResourceOptions(provider=provider, delete_before_replace=True),
        )
    )()

    service_name = f"{full_name}-service"
    back_service = k8s.Service(
        resource_name=service_name,
        api_version="v1",
        kind="Service",
        metadata=ObjectMetaArgs(
            name=service_name,
            labels=labels,
        ),
        spec=k8s.ServiceSpecArgs(
            type=service_config["type"],
            ports=[
                k8s.ServicePortArgs(
                    port=service_config["port"],
                    name=f"{service_name}-port",
                    target_port=container_port_name,
                    node_port=service_config["nodePort"],
                )
            ],
            selector=labels,
        ),
        opts=ResourceOptions(provider=provider, delete_before_replace=True),
    )

    back_host = service_name
    back_port = service_config["port"]
    return back_host, back_port


def mk_test(test_config, environment, back_host, back_port, provider=None):
    name = test_config["name"]
    full_name = mk_full_name(environment=environment, name=name)
    container_config = test_config["deployment"]["container"]
    service_config = test_config["service"]
    labels = {
        "environment": environment,
    }

    test_config = container_config["test"]
    test_config["file"]["app"]["host"] = back_host
    test_config["file"]["app"]["port"] = back_port
    test_config["file"]["users"] = []

    app_config_map = (
        lambda name=f"{full_name}-configmap": k8s.ConfigMap(
            resource_name=name,
            api_version="v1",
            kind="ConfigMap",
            metadata=ObjectMetaArgs(
                name=name,
                labels=labels,
            ),
            data={
                test_config["filePath"]: str(yaml.dump(test_config["file"])),
            },
            opts=ResourceOptions(provider=provider, delete_before_replace=True),
        )
    )()

    container_port = container_config["port"]
    deployment = (
        lambda name_=f"{full_name}-job": Job (
            resource_name=name_,
            api_version="batch/v1",
            kind="Job",
            metadata=ObjectMetaArgs(name=name_, labels=labels),
            spec=JobSpecArgs(
                template=k8s.PodTemplateSpecArgs(
                    metadata=ObjectMetaArgs(
                        labels=labels,
                    ),
                    spec=(
                        lambda config_volume=f"{name_}-config-volume", configMountPath=container_config[
                            "test"
                        ][
                            "mountPath"
                        ]: k8s.PodSpecArgs(
                            restart_policy="OnFailure",
                            containers=[
                                (
                                    lambda container_name=container_config[
                                        "name"
                                    ], docker_hub_image=container_config[
                                        "dockerHubImage1"
                                    ]: k8s.ContainerArgs(
                                        name=container_name,
                                        image=docker_hub_image,
                                        image_pull_policy="Always",
                                        ports=[
                                            k8s.ContainerPortArgs(
                                                container_port=container_port,
                                                name=f"{name}-port",
                                            )
                                        ],
                                        volume_mounts=[
                                            k8s.VolumeMountArgs(
                                                mount_path=configMountPath,
                                                name=config_volume,
                                            ),
                                        ],
                                        env=[
                                            k8s.EnvVarArgs(
                                                name=test_config["varName"],
                                                value=f"{configMountPath}/{test_config['filePath']}",
                                            ),
                                        ],
                                        resources=k8s.ResourceRequirementsArgs(
                                            requests={
                                                "memory": "64Mi",
                                                "cpu": "250m",
                                            },
                                            limits={
                                                "memory": "128Mi",
                                                "cpu": "500m",
                                            },
                                        ),
                                    )
                                )()
                            ],
                            volumes=[
                                k8s.VolumeArgs(
                                    name=config_volume,
                                    config_map=k8s.ConfigMapVolumeSourceArgs(
                                        name=app_config_map.metadata.name
                                    ),
                                )
                            ],
                        )
                    )(),
                ),
            ),
            opts=ResourceOptions(provider=provider, delete_before_replace=True),
        )
    )()


def mk_setup(environment, provider=None):
    config = pulumi.Config(environment)
    postgres_host, postgres_port = mk_postgres(
        postgres_config=config.require_object("postgres"),
        environment=environment,
        provider=provider,
    )

    back_host, back_port = mk_back(
        back_config=config.require_object("back"),
        environment=environment,
        postgres_host=postgres_host,
        postgres_port=postgres_port,
        provider=provider,
    )

    if environment == "dev":
        mk_test(
            test_config=config.require_object("test"),
            environment=environment,
            back_host=back_host,
            back_port=back_port,
            provider=provider,
        )


# provider = Provider("k8s-yaml-rendered", render_yaml_to_directory="yaml")
provider = None
if pulumi.get_stack() == "dev":
    mk_setup(environment="dev", provider=provider)
elif pulumi.get_stack() == "prod":
    mk_setup(environment="prod", provider=provider)

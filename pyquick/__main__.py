"""
Creating a Kubernetes Deployment
"""
import pulumi
from pulumi_kubernetes.apps.v1 import Deployment
from pulumi_kubernetes.core.v1 import Service, ConfigMap
from pulumi_kubernetes.meta.v1 import ObjectMetaArgs
from pulumi_kubernetes.provider import Provider
from pulumi import ResourceOptions

# Minikube does not implement services of type `LoadBalancer`; require the user to specify if we're
# running on minikube, and if so, create only services of type ClusterIP.
config = pulumi.Config()
is_minikube = config.require_bool("isMinikube")

app_name = "nginx"
app_labels = {"app": app_name}


# Instantiate a Kubernetes Provider and specify the render directory.
provider = Provider("render-yaml", render_yaml_to_directory="rendered");

deployment = Deployment(
    app_name,
    spec={
        "selector": {"match_labels": app_labels},
        "replicas": 1,
        "template": {
            "metadata": {"labels": app_labels},
            "spec": {"containers": [{"name": app_name, "image": "nginx"}]},
        },
    },
    opts=ResourceOptions(provider=provider)
)

# Allocate an IP to the Deployment.
frontend = Service(
    app_name,
    metadata={
        "labels": deployment.spec["template"]["metadata"]["labels"],
    },
    spec={
        "type": "ClusterIP" if is_minikube else "LoadBalancer",
        "ports": [{"port": 80, "target_port": 80, "protocol": "TCP"}],
        "selector": app_labels,
    },
)

# configMap = ConfigMap(
#     metadata=ObjectMetaArgs(
#         name="some_name", labels={"environment": "dev", "app": "some_app"}
#     ),
#     data={"file": "contents"},
#     resource_name="myConfigMap",
# )

# When "done", this will print the public IP.
result = None
if is_minikube:
    result = frontend.spec.apply(
        lambda v: v["cluster_ip"] if "cluster_ip" in v else None
    )
else:
    ingress = frontend.status.apply(
        lambda v: v["load_balancer"]["ingress"][0] if "load_balancer" in v else None
    )
    if ingress is not None:
        result = ingress.apply(lambda v: v["ip"] if "ip" in v else v["hostname"])


pulumi.export("ip", result)

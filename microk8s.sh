export KUBECONFIG_DIR="$PWD/.kube"
export KUBECONFIG="$KUBECONFIG_DIR/config"
mkdir -p $KUBECONFIG_DIR
microk8s config > $KUBECONFIG
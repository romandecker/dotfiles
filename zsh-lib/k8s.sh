kp() {
	local service=$(kubectl get services --no-headers | fzf | awk '{print $1;}')
	print -z "kubectl port-forward services/$service 54321:5432"
}

kc() {
	local context=$(kubectl config get-contexts --no-headers | sed 's/^[ *]*//g' | fzf | awk '{print $1;}')
	print -z "kubectl config use-context $context"
}
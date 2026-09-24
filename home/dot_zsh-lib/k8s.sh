kp() {
	local service=$(kubectl get services --no-headers | fzf | awk '{print $1;}')
	print -z "kubectl port-forward services/$service 54321:5432"
}

kc() {
	local contexts
	contexts=$(kubectl config get-contexts --no-headers 2>/dev/tty)
	if [ $? -ne 0 ] || [ -z "$contexts" ]; then
		return 1
	fi
	local context=$(echo "$contexts" | sed 's/^[ *]*//g' | fzf | awk '{print $1;}')
	print -z "kubectl config use-context $context && kubectl cluster-info"
}
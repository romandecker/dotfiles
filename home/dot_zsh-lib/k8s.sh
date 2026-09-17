kp() {
	local service=$(kubectl get services --no-headers | fzf | awk '{print $1;}')
	print -z "kubectl port-forward services/$service 54321:5432"
}

kc() {
	local contexts
	local err
	err=$(mktemp)
	contexts=$(kubectl config get-contexts --no-headers 2>"$err")
	if grep -q "Please visit this URL" "$err"; then
		cat "$err"
		rm -f "$err"
		return 1
	fi
	rm -f "$err"
	local context=$(echo "$contexts" | sed 's/^[ *]*//g' | fzf | awk '{print $1;}')
	print -z "kubectl config use-context $context && kubectl cluster-info"
}
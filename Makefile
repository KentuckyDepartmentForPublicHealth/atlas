git:
	git pull
	git add .
	@echo "$$m" > .commit_msg.tmp
	git commit -F .commit_msg.tmp
	@rm -f .commit_msg.tmp
	git push
	git status
	git log

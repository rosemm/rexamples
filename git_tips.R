# git and github set up stuff

# connecting a project to an existing repo
system("git remote add origin https://github.com/rosemm/rexamples.git")
system("git pull")
system("git remote -v")
system("git push -u origin master") # https://landeco2point0.wordpress.com/2014/07/22/things-i-forget-pushpull-greyed-out-in-rstudio/
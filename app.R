# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file


my_cred <- git2r::cred_user_pass("martin", "wzfxjxtv55g5mbbgybbjml2opsgcjnupcoav7giwfyyxl45kuvwa")
usethis::use_git_protocol("https")
usethis::use_git_credentials(my_cred)

remotes::install_git('https://martin@dev.azure.com/inndx/taxadmin/_git/inndxtdr', ref = "dev", git = 'git2r', credentials = my_cred)

pkgload::load_all()
options( "golem.app.prod" = TRUE)
inndxweb::run_app() # add parameters here (if any)

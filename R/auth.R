# Single-password gate. The app is reachable on the public internet, so the
# UI shows a login overlay and no data is read until the password matches.
#
# APP_PASSWORD_SALT  any random string
# APP_PASSWORD_HASH  password_hash("<your password>", "<the salt>")
# APP_AUTH=off       disables the gate (local development only)
#
# After a successful login the browser keeps a token (derived from the hash)
# so you don't type the password at the gym. Changing the password or the
# salt logs every device out.

password_hash <- function(password, salt) {
  as.character(openssl::sha256(paste0(salt, ":", password)))
}

auth_enabled <- function() !identical(Sys.getenv("APP_AUTH"), "off")

auth_check_password <- function(password) {
  hash <- Sys.getenv("APP_PASSWORD_HASH")
  nzchar(hash) && !is.null(password) &&
    identical(password_hash(password, Sys.getenv("APP_PASSWORD_SALT")), hash)
}

auth_token <- function() {
  as.character(openssl::sha256(paste0("remember:", Sys.getenv("APP_PASSWORD_SALT"), ":",
                                      Sys.getenv("APP_PASSWORD_HASH"))))
}

auth_check_token <- function(token) {
  nzchar(Sys.getenv("APP_PASSWORD_HASH")) && is.character(token) && identical(token, auth_token())
}

login_ui <- function() {
  shiny::div(
    id = "login-overlay",
    shiny::div(
      class = "login-card",
      shiny::h2("5/3/1"),
      shiny::passwordInput("login_password", NULL, placeholder = "Password", width = "100%"),
      shiny::actionButton("login_submit", "Log in", class = "btn-primary btn-lg w-100"),
      shiny::div(id = "login-error", class = "text-danger mt-2", shiny::textOutput("login_error", inline = TRUE))
    )
  )
}

# Calls `on_login` once, when the user is authenticated.
auth_server <- function(input, output, session, on_login) {
  started <- FALSE
  login <- function() {
    if (started) return()
    started <<- TRUE
    shiny::removeUI("#login-overlay", immediate = TRUE, session = session)
    on_login()
  }

  if (!auth_enabled()) {
    login()
    return(invisible())
  }
  if (!nzchar(Sys.getenv("APP_PASSWORD_HASH"))) {
    output$login_error <- shiny::renderText("APP_PASSWORD_HASH is not set on the server.")
    return(invisible())
  }

  shiny::observeEvent(input$auth_token, {
    if (auth_check_token(input$auth_token)) login()
  })
  shiny::observeEvent(input$login_submit, {
    if (auth_check_password(input$login_password)) {
      session$sendCustomMessage("auth-remember", list(token = auth_token()))
      login()
    } else {
      output$login_error <- shiny::renderText("Wrong password.")
    }
  })
}

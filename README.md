# Common Lisp web app example

An example of how to build a modern web application with Common Lisp.

## Design decisions

These tenets reflect my personal preferences and are not intended to be taken as universal best practices.

- **Use Common Lisp as much as possible**
  Use technologies conducive to writing as much of the application as possible in Common Lisp:
  - **HTML** - Use the Spinneret package to write HTML as s-expressons.
  - **CSS** - Use Tailwind for inline styles.
  - **JavaScript** - Use HTMX and Hyperscript for client-side interactivity and network requests.

- **Reproducible builds and portable environment**
  Use nix flakes to provide a portable build process, development environment and Docker images for release.

## Usage

Connect to a REPL with your prefered IDE and run the following, ensuring that the REPL working directory is set to the project root:

```lisp
(asdf:load-system 'cl-web-app-example)
(cl-web-app-example:main)
```

The server will listen on port `5000`.

Start a Tailwind watch process with `tailwindcss -w -i ./tailwind.css -o public/tailwind.css` to monitor changes in Lisp source and regenerate the stylesheet.

The nix flake also provides some helper scripts:

- `run-server` - Run the server.
- `run-tests` - Run unit tests.
- `docker-build` - Build a docker image of the server.
- `docker-run` - Run the server in the container.

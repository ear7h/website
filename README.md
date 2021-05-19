# website

My personal website

## architecture

* `compiler/` - compiles static files like web pages and service configs
* `services/` - services for the website
	* `http-git` - a read-only http/html git server
	* `http-static` - a simple and easily configured http server
* `posts/` - blog style post
	* **note** the front-matter is formatted as http headers vs the usual yaml
* `home/` - root directory files

### todo
* home navigation
* css
* home pages

all: \
  _site/app-form.html \
  _site/blog-post.html


_site/%.html:
	mkdir -p $(dir $@)
	runghc -isrc/ bin/generate.hs --pretty $* > $@

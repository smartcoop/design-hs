all: \
  _site/index.html \
  _site/app-empty.html \
  _site/app-navigation.html \
  _site/app-toolbar.html \
  _site/app-titlebar.html \
  _site/app-form.html \
  _site/app-form--banner.html \
  _site/app-form--wizard.html \
  _site/app-form--side-menu.html \
  _site/app-dialog.html \
  _site/datagrid.html \
  _site/registration.html \
  _site/web-empty.html \
  _site/blog-post.html \
  _site/tools-new-contract.html


_site/tools-new-contract.html: src/Smart/Html/Application.hs src/Smart/Html/Tools.hs
	mkdir -p $(dir $@)
	runghc -isrc/ bin/generate.hs --pretty tools-new-contract > $@

_site/%.html: src/Smart/Html/Application.hs src/Smart/Html/Website.hs
	mkdir -p $(dir $@)
	runghc -isrc/ bin/generate.hs --pretty $* > $@

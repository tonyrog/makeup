###
##  Build of all common DTD state machines
##

DTDS = \
	"-//W3C//DTD XMLSCHEMA 200102//EN" \
	"-//W3C//DTD XHTML 1.0 Strict//EN" \
	"-//W3C//DTD XHTML 1.0 Transitional//EN" \
	"-//W3C//DTD XHTML 1.0 Frameset//EN" \
	"-//W3C//DTD XHTML 1.1//EN" \
	"-//IETF//DTD HTML 2.0//EN" \
	"-//IETF//DTD HTML Strict Level 1//EN" \
	"-//IETF//DTD HTML Level 1//EN"	\
	"-//IETF//DTD HTML 2.0 Level 1//EN" \
	"-//IETF//DTD HTML 2.0 Strict//EN" \
	"-//W3C//DTD HTML 3.2 Final//EN" \
	"-//W3C//DTD HTML 4.0 Transitional//EN" \
	"-//W3C//DTD HTML 4.0//EN" \
	"-//W3C//DTD HTML 4.0 Frameset//EN" \
	"-//W3C//DTD HTML 4.01 Transitional//EN" \
	"-//W3C//DTD HTML 4.01//EN" \
	"-//W3C//DTD HTML 4.01 Frameset//EN"

MODULES = \
	xmlschema_dtd \
	xhtml1_strict_dtd \
	xhtml1_transitional_dtd \
	xhtml1_frameset_dtd \
	xhtml11_dtd \
	html2_dtd \
	html2_1s_dtd \
	html2_1_dtd \
	html2_s_dtd \
	html32_dtd \
	html40_loose_dtd \
	html40_strict_dtd \
	html40_framset_dtd \
	html4_loose_dtd \
	html4_strict_dtd \
	html4_frameset_dtd \
	property_list10_dtd

WBXML = \
	browser_settings \
	syncml_settings \
	wv_settings \
	wml_11 \
	wml_12 \
	wml_13 \
	si_10 \
	sl_10 \
	prov_old_10 \
	prov_10 \
	msprov_3 \
	msprov_5 \
	uaprof \
	devinf_10 \
	devinf_101 \
	devinf_11 \
	syncml_10 \
	syncml_101 \
	syncml_11 \
	syncml_112 \
	syncml_12

OBJS = $(MODULES:%=./ebin/%.beam) $(WBXML:%=./ebin/wbxml_%.beam)

MAKEUP = ./makeup

all: $(OBJS)

debug: all

depend:



generate: public.map
	@echo "# generated" > make.inc
	@for dtd in $(DTDS) ; do \
		MOD=`$(MAKEUP) config -m "$$dtd"`; \
		OPTS=`$(MAKEUP) config -o "$$dtd"`; \
		DTD=`$(MAKEUP) config -d "$$dtd"`; \
		echo "# $$dtd" >> make.inc; \
		echo "src/$$MOD.erl: $$DTD" >> make.inc; \
		echo "	$(MAKEUP) compile $$OPTS -o ./src -m $$MOD $$DTD" >> make.inc; \
	done

# -//W3C//DTD XMLSCHEMA 200102//EN
# USES Namespace compilation!!! Not 100%
src/xmlschema_dtd.erl: xmlschema/XMLSchema.dtd
	$(MAKEUP) compile -n -m xmlschema_dtd -o ./src xmlschema/XMLSchema.dtd
# -//W3C//DTD XHTML 1.0 Strict//EN
src/xhtml1_strict_dtd.erl: xhtml1/xhtml1-strict.dtd
	$(MAKEUP) compile -m xhtml1_strict_dtd  -o ./src xhtml1/xhtml1-strict.dtd
# -//W3C//DTD XHTML 1.0 Transitional//EN
src/xhtml1_transitional_dtd.erl: xhtml1/xhtml1-transitional.dtd
	$(MAKEUP) compile -m xhtml1_transitional_dtd  -o ./src xhtml1/xhtml1-transitional.dtd
# -//W3C//DTD XHTML 1.0 Frameset//EN
src/xhtml1_frameset_dtd.erl: xhtml1/xhtml1-frameset.dtd
	$(MAKEUP) compile -m xhtml1_frameset_dtd  -o ./src xhtml1/xhtml1-frameset.dtd
# -//W3C//DTD XHTML 1.1//EN
src/xhtml11_dtd.erl: xhtml11/xhtml11.dtd
	$(MAKEUP) compile -m xhtml11_dtd  -o ./src xhtml11/xhtml11.dtd
# -//IETF//DTD HTML 2.0//EN
src/html2_dtd.erl: html2/html.dtd
	$(MAKEUP) compile -m html2_dtd -I  -o ./src html2/html.dtd
# -//IETF//DTD HTML Strict Level 1//EN
src/html2_1s_dtd.erl: html2/html-1s.dtd
	$(MAKEUP) compile -m html2_1s_dtd -I  -o ./src html2/html-1s.dtd
# -//IETF//DTD HTML 2.0 Level 1//EN
src/html2_1_dtd.erl: html2/html-1.dtd
	$(MAKEUP) compile -m html2_1_dtd -I  -o ./src html2/html-1.dtd
# -//IETF//DTD HTML 2.0 Strict//EN
src/html2_s_dtd.erl: html2/html-s.dtd
	$(MAKEUP) compile -m html2_s_dtd -I  -o ./src html2/html-s.dtd
# -//W3C//DTD HTML 3.2 Final//EN
src/html32_dtd.erl: html32/html32.dtd
	$(MAKEUP) compile -m html32_dtd -I  -o ./src html32/html32.dtd
# -//W3C//DTD HTML 4.0 Transitional//EN
src/html40_loose_dtd.erl: html40/loose.dtd
	$(MAKEUP) compile -m html40_loose_dtd -I  -o ./src html40/loose.dtd
# -//W3C//DTD HTML 4.0//EN
src/html40_strict_dtd.erl: html40/strict.dtd
	$(MAKEUP) compile -m html40_strict_dtd -I  -o ./src html40/strict.dtd
# -//W3C//DTD HTML 4.0 Frameset//EN
src/html40_framset_dtd.erl: html40/frameset.dtd
	$(MAKEUP) compile -m html40_framset_dtd -I  -o ./src html40/frameset.dtd
# -//W3C//DTD HTML 4.01 Transitional//EN
src/html4_loose_dtd.erl: html4/loose.dtd
	$(MAKEUP) compile -m html4_loose_dtd -I  -o ./src html4/loose.dtd
# -//W3C//DTD HTML 4.01//EN
src/html4_strict_dtd.erl: html4/strict.dtd
	$(MAKEUP) compile -m html4_strict_dtd -I  -o ./src html4/strict.dtd
# -//W3C//DTD HTML 4.01 Frameset//EN
src/html4_frameset_dtd.erl: html4/frameset.dtd
	$(MAKEUP) compile -m html4_frameset_dtd -I  -o ./src html4/frameset.dtd
# -//Apple Computer//DTD PLIST 1.0//EN
src/property_list10_dtd.erl: apple/PropertyList-1.0.dtd
	$(MAKEUP) compile -m property_list10_dtd -o ./src apple/PropertyList-1.0.dtd

ebin/wbxml_%.beam:	src/wbxml_%.erl
	erlc -W -o ./ebin $(ERLC_FLAGS) $<

.PRECIOUS:	src/wbxml_%.erl
src/wbxml_%.erl:	wbxml/%.tab
	$(MAKEUP) wbxml -o ./src $<

ebin/%.beam:	src/%.erl
	erlc -W -o ./ebin $(ERLC_FLAGS) $<

src/%.erl:	wbxml/%.tab
	$(MAKEUP) wbxml -o ./src $<

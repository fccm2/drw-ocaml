RMDIR = rm -rf
dl: mini-svg-0.03.9b.zip
mini-svg-0.03.9b.zip:
	wget http://decapode314.free.fr/ocaml/mini-svg3/dl/mini-svg-0.03.9b.zip
unar:
	unzip mini-svg-0.03.9b.zip
	$(RM) mini-svg-0.03.9b.zip
patch:
	cd mini-svg-0.03.9b && \
	 patch -p1 < ../_pa/mini-svg-0.03.9b-c.patch && \
	 patch -p1 < ../_pa/mini-svg-0.03.9c-d.patch && \
	 cd .. && mv mini-svg-0.03.9b mini-svg-0.03.9d && \
	 cd mini-svg-0.03.9d/src && $(MAKE)
clean:
	$(RM) mini-svg-0.03.9b.zip
	$(RMDIR) mini-svg-0.03.9d/

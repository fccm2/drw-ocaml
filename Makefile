dep:
	$(MAKE) -f dep.mk dl
	$(MAKE) -f dep.mk unar
	$(MAKE) -f dep.mk patch
clean:
	$(MAKE) -f dep.mk clean
	$(MAKE) -C _drw -f drw.mk clean

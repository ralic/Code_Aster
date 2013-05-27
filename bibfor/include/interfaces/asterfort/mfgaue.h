        interface
          subroutine mfgaue(fid,typgeo,refcoo,modeco,ngauss,gscoo,wg,&
     &locname,ndim,nomasu,cret)
            integer :: fid
            integer :: typgeo
            real(kind=8) :: refcoo(*)
            integer :: modeco
            integer :: ngauss
            real(kind=8) :: gscoo(*)
            real(kind=8) :: wg(*)
            character(*) :: locname
            integer :: ndim
            character(*) :: nomasu
            integer :: cret
          end subroutine mfgaue
        end interface

        interface
          subroutine avdowh(nbvec,nbordr,nommat,nomcri,ncycl,gdeq,&
     &grdvie,forvie,post,domel,nrupt)
            integer :: nbordr
            integer :: nbvec
            character(len=8) :: nommat
            character(len=16) :: nomcri
            integer :: ncycl(nbvec)
            real(kind=8) :: gdeq(nbvec*nbordr)
            character(len=8) :: grdvie
            character(len=16) :: forvie
            logical :: post
            real(kind=8) :: domel(nbvec*nbordr)
            real(kind=8) :: nrupt(nbvec*nbordr)
          end subroutine avdowh
        end interface

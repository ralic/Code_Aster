        interface
          subroutine ircnc8(ifi,nbno,prno,nueq,nec,dg,ncmpmx,vale,&
     &nomcmp,nomnoe,lcor,ndim,coor,numnoe,nbcmpt,nucmpu,lsup,borsup,linf&
     &,borinf,lmax,lmin,formr)
            integer :: ifi
            integer :: nbno
            integer :: prno(*)
            integer :: nueq(*)
            integer :: nec
            integer :: dg(*)
            integer :: ncmpmx
            complex(kind=8) :: vale(*)
            character(*) :: nomcmp(*)
            character(*) :: nomnoe(*)
            logical :: lcor
            integer :: ndim
            real(kind=8) :: coor(*)
            integer :: numnoe(*)
            integer :: nbcmpt
            integer :: nucmpu(*)
            logical :: lsup
            real(kind=8) :: borsup
            logical :: linf
            real(kind=8) :: borinf
            logical :: lmax
            logical :: lmin
            character(*) :: formr
          end subroutine ircnc8
        end interface

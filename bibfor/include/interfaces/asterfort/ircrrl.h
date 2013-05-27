        interface
          subroutine ircrrl(ifi,nbno,desc,nec,dg,ncmpmx,vale,nomcmp,&
     &nomnoe,lcor,ndim,coor,numnoe,nbcmpt,nucmpu,lsup,borsup,linf,borinf&
     &,lmax,lmin,formr)
            integer :: ifi
            integer :: nbno
            integer :: desc(*)
            integer :: nec
            integer :: dg(*)
            integer :: ncmpmx
            real(kind=8) :: vale(*)
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
          end subroutine ircrrl
        end interface

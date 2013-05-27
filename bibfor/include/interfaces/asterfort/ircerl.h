        interface
          subroutine ircerl(ifi,nbel,ligrel,nbgrel,longr,ncmpmx,vale,&
     &nomcmp,nomel,loc,celd,connex,point,nomnos,nbcmpt,nucmpu,nbnot,&
     &numnoe,nbmat,nummai,lsup,borsup,linf,borinf,lmax,lmin,lcor,ndim,&
     &coor,nolili,formr,ncmpv,nucmp)
            integer :: ifi
            integer :: nbel
            integer :: ligrel(*)
            integer :: nbgrel
            integer :: longr(*)
            integer :: ncmpmx
            real(kind=8) :: vale(*)
            character(*) :: nomcmp(*)
            character(*) :: nomel(*)
            character(*) :: loc
            integer :: celd(*)
            integer :: connex(*)
            integer :: point(*)
            character(*) :: nomnos(*)
            integer :: nbcmpt
            integer :: nucmpu(*)
            integer :: nbnot
            integer :: numnoe(*)
            integer :: nbmat
            integer :: nummai(*)
            logical :: lsup
            real(kind=8) :: borsup
            logical :: linf
            real(kind=8) :: borinf
            logical :: lmax
            logical :: lmin
            logical :: lcor
            integer :: ndim
            real(kind=8) :: coor(*)
            character(len=19) :: nolili
            character(*) :: formr
            integer :: ncmpv
            integer :: nucmp(*)
          end subroutine ircerl
        end interface

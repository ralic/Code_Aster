        interface
          subroutine irceca(ifi,ligrel,nbgrel,longr,ncmpmx,vale,nomgd,&
     &ncmpgd,celd,nbnoma,typma,nomsym,nbmat,lresu,nbcput,ncmput,imodl,&
     &ncmpv,nucmpv,nive)
            integer :: ifi
            integer :: ligrel(*)
            integer :: nbgrel
            integer :: longr(*)
            integer :: ncmpmx
            real(kind=8) :: vale(*)
            character(*) :: nomgd
            character(*) :: ncmpgd(*)
            integer :: celd(*)
            integer :: nbnoma(*)
            integer :: typma(*)
            character(*) :: nomsym
            integer :: nbmat
            logical :: lresu
            integer :: nbcput
            character(*) :: ncmput(*)
            integer :: imodl
            integer :: ncmpv
            integer :: nucmpv(*)
            integer :: nive
          end subroutine irceca
        end interface

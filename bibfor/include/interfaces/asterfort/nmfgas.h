        interface
          subroutine nmfgas(fami,npg,icodma,pgl,nno,nc,ugl,effnom,pm,&
     &crit,tmoins,tplus,xlong0,a,coeffl,irram,irrap,kls,flc,effnoc,pp)
            character(*) :: fami
            integer :: npg
            integer :: icodma
            real(kind=8) :: pgl(3,3)
            integer :: nno
            integer :: nc
            real(kind=8) :: ugl(12)
            real(kind=8) :: effnom
            real(kind=8) :: pm(3)
            real(kind=8) :: crit(*)
            real(kind=8) :: tmoins
            real(kind=8) :: tplus
            real(kind=8) :: xlong0
            real(kind=8) :: a
            real(kind=8) :: coeffl(7)
            real(kind=8) :: irram
            real(kind=8) :: irrap
            real(kind=8) :: kls(78)
            real(kind=8) :: flc
            real(kind=8) :: effnoc
            real(kind=8) :: pp(3)
          end subroutine nmfgas
        end interface

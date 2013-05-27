        interface
          subroutine hbrcvx(sig,vid,nmat,materf,seuil,vp,vecp)
            integer :: nmat
            real(kind=8) :: sig(6)
            real(kind=8) :: vid(3)
            real(kind=8) :: materf(nmat,2)
            real(kind=8) :: seuil
            real(kind=8) :: vp(3)
            real(kind=8) :: vecp(3,3)
          end subroutine hbrcvx
        end interface

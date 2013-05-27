        interface
          subroutine assma2(lmasym,tt,nu14,ncmp,matel,c1,jvalm,jtmp2,&
     &lgtmp2)
            logical :: lmasym
            character(len=2) :: tt
            character(len=14) :: nu14
            integer :: ncmp
            character(len=19) :: matel
            real(kind=8) :: c1
            integer :: jvalm(2)
            integer :: jtmp2
            integer :: lgtmp2
          end subroutine assma2
        end interface

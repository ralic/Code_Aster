        interface
          subroutine coefmo(typflu,zrigi,nbm,nmode,indic,x,pulsc,vgap,&
     &xsi0,veci1,vecr1,vecr2,vecr3,vecr4,vecr5,xmf,xkf,xcf)
            character(len=8) :: typflu
            logical :: zrigi
            integer :: nbm
            integer :: nmode
            integer :: indic
            real(kind=8) :: x(2)
            real(kind=8) :: pulsc
            real(kind=8) :: vgap
            real(kind=8) :: xsi0
            integer :: veci1(*)
            real(kind=8) :: vecr1(*)
            real(kind=8) :: vecr2(*)
            real(kind=8) :: vecr3(*)
            real(kind=8) :: vecr4(*)
            real(kind=8) :: vecr5(*)
            real(kind=8) :: xmf
            complex(kind=8) :: xkf
            real(kind=8) :: xcf
          end subroutine coefmo
        end interface

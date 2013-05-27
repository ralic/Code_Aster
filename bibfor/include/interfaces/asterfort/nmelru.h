        interface
          subroutine nmelru(fami,kpg,ksp,poum,imate,compor,epseq,p,&
     &divu,nonlin,ener)
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            character(*) :: poum
            integer :: imate
            character(len=16) :: compor(*)
            real(kind=8) :: epseq
            real(kind=8) :: p
            real(kind=8) :: divu
            logical :: nonlin
            real(kind=8) :: ener(2)
          end subroutine nmelru
        end interface

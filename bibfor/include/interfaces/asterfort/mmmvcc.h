        interface
          subroutine mmmvcc(phasep,nnl,wpg,ffl,jacobi,jeu,coefac,&
     &dlagrc,vectcc)
            character(len=9) :: phasep
            integer :: nnl
            real(kind=8) :: wpg
            real(kind=8) :: ffl(9)
            real(kind=8) :: jacobi
            real(kind=8) :: jeu
            real(kind=8) :: coefac
            real(kind=8) :: dlagrc
            real(kind=8) :: vectcc(9)
          end subroutine mmmvcc
        end interface

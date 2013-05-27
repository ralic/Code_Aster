        interface
          subroutine xddlim(modele,motcle,nomn,ino,valimr,valimc,&
     &valimf,fonree,icompt,lisrel,ndim,direct,jnoxfv,ch1,ch2,ch3,cnxinv)
            character(len=8) :: modele
            character(len=8) :: motcle
            character(len=8) :: nomn
            integer :: ino
            real(kind=8) :: valimr
            complex(kind=8) :: valimc
            character(len=8) :: valimf
            character(len=4) :: fonree
            integer :: icompt
            character(len=19) :: lisrel
            integer :: ndim
            real(kind=8) :: direct(3)
            integer :: jnoxfv
            character(len=19) :: ch1
            character(len=19) :: ch2
            character(len=19) :: ch3
            character(len=19) :: cnxinv
          end subroutine xddlim
        end interface

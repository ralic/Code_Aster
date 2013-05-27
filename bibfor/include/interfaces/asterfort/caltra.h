        interface
          subroutine caltra(np1,np4,nbm,nfour,ttrans,ttran0,vitgtr,&
     &depgtr,vitg0,depg0,masgi,amor,puls,pulsd,mtrans,s0,z0,sr0,za1,za2,&
     &za3,za4,za5,zitr,zin,fextt0,fexttr,dttr,omegaf,aa,bb,ntrans)
            integer :: np4
            integer :: np1
            integer :: nbm
            integer :: nfour
            real(kind=8) :: ttrans
            real(kind=8) :: ttran0
            real(kind=8) :: vitgtr(*)
            real(kind=8) :: depgtr(*)
            real(kind=8) :: vitg0(*)
            real(kind=8) :: depg0(*)
            real(kind=8) :: masgi(*)
            real(kind=8) :: amor(*)
            real(kind=8) :: puls(*)
            real(kind=8) :: pulsd(*)
            real(kind=8) :: mtrans(2,2,*)
            complex(kind=8) :: s0(*)
            complex(kind=8) :: z0(*)
            complex(kind=8) :: sr0(*)
            complex(kind=8) :: za1(*)
            complex(kind=8) :: za2(*)
            complex(kind=8) :: za3(*)
            complex(kind=8) :: za4(np4,*)
            complex(kind=8) :: za5(np4,*)
            complex(kind=8) :: zitr(*)
            complex(kind=8) :: zin(*)
            real(kind=8) :: fextt0(*)
            real(kind=8) :: fexttr(*)
            real(kind=8) :: dttr
            real(kind=8) :: omegaf(*)
            real(kind=8) :: aa(np4,*)
            real(kind=8) :: bb(np4,*)
            integer :: ntrans
          end subroutine caltra
        end interface

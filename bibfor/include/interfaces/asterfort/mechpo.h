        interface
          subroutine mechpo(souche,charge,modele,chdep2,chdynr,suropt,&
     &lpain,lchin,nbopt,typcoe,alpha,calpha)
            character(*) :: souche
            character(*) :: charge
            character(*) :: modele
            character(*) :: chdep2
            character(*) :: chdynr
            character(*) :: suropt
            character(*) :: lpain(*)
            character(*) :: lchin(*)
            integer :: nbopt
            character(*) :: typcoe
            real(kind=8) :: alpha
            complex(kind=8) :: calpha
          end subroutine mechpo
        end interface

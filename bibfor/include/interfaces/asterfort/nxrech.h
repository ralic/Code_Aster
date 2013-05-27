        interface
          subroutine nxrech(modele,mate,carele,charge,infoch,numedd,&
     &time,lonch,compor,vtempm,vtempp,vtempr,vtemp,vhydr,vhydrp,tmpchi,&
     &tmpchf,vec2nd,cnvabt,cnresi,rho,iterho,parmer,parmei)
            character(len=24) :: modele
            character(len=24) :: mate
            character(len=24) :: carele
            character(len=24) :: charge
            character(len=24) :: infoch
            character(len=24) :: numedd
            character(len=24) :: time
            integer :: lonch
            character(len=24) :: compor
            character(len=24) :: vtempm
            character(len=24) :: vtempp
            character(len=24) :: vtempr
            character(len=24) :: vtemp
            character(len=24) :: vhydr
            character(len=24) :: vhydrp
            character(len=24) :: tmpchi
            character(len=24) :: tmpchf
            character(len=24) :: vec2nd
            character(len=24) :: cnvabt
            character(len=24) :: cnresi
            real(kind=8) :: rho
            integer :: iterho
            real(kind=8) :: parmer(2)
            integer :: parmei(2)
          end subroutine nxrech
        end interface

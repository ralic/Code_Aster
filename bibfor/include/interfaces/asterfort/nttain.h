        interface
          subroutine nttain(modele,mate,carele,charge,infoch,numedd,&
     &solveu,time,epsr,lonch,matass,maprec,cnchci,cnresi,vtemp,vtempm,&
     &vtempp,vec2nd,chlapm,chlapp,ci1,ci2,testi)
            character(len=24) :: modele
            character(len=24) :: mate
            character(len=24) :: carele
            character(len=24) :: charge
            character(len=24) :: infoch
            character(len=24) :: numedd
            character(len=19) :: solveu
            character(len=24) :: time
            real(kind=8) :: epsr
            integer :: lonch
            character(len=24) :: matass
            character(len=19) :: maprec
            character(len=24) :: cnchci
            character(len=24) :: cnresi
            character(len=24) :: vtemp
            character(len=24) :: vtempm
            character(len=24) :: vtempp
            character(len=24) :: vec2nd
            character(len=24) :: chlapm
            character(len=24) :: chlapp
            character(len=1) :: ci1
            character(len=1) :: ci2
            real(kind=8) :: testi
          end subroutine nttain
        end interface

        interface
          subroutine nxnewt(modele,mate,carele,charge,infcha,infoch,&
     &numedd,solveu,time,lonch,matass,maprec,cnchci,vtemp,vtempm,vtempp,&
     &vec2nd,mediri,conver,vhydr,vhydrp,tmpchi,tmpchf,compor,cnvabt,&
     &cnresi,parcri,parcrr,reasma,testr,testm)
            character(len=24) :: modele
            character(len=24) :: mate
            character(len=24) :: carele
            character(len=24) :: charge
            character(len=19) :: infcha
            character(len=24) :: infoch
            character(len=24) :: numedd
            character(len=19) :: solveu
            character(len=24) :: time
            integer :: lonch
            character(len=24) :: matass
            character(len=19) :: maprec
            character(len=24) :: cnchci
            character(len=24) :: vtemp
            character(len=24) :: vtempm
            character(len=24) :: vtempp
            character(len=24) :: vec2nd
            character(len=24) :: mediri
            logical :: conver
            character(len=24) :: vhydr
            character(len=24) :: vhydrp
            character(len=24) :: tmpchi
            character(len=24) :: tmpchf
            character(len=24) :: compor
            character(len=24) :: cnvabt
            character(len=24) :: cnresi
            integer :: parcri(3)
            real(kind=8) :: parcrr(2)
            logical :: reasma
            real(kind=8) :: testr
            real(kind=8) :: testm
          end subroutine nxnewt
        end interface

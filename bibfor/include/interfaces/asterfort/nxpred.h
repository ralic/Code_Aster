        interface
          subroutine nxpred(modele,mate,carele,charge,infoch,numedd,&
     &solveu,lostat,time,lonch,matass,maprec,vtemp,vtempm,vtempp,vhydr,&
     &vhydrp,tmpchi,tmpchf,compor,cndirp,cnchci,vec2nd,vec2ni)
            character(len=24) :: modele
            character(len=24) :: mate
            character(len=24) :: carele
            character(len=24) :: charge
            character(len=24) :: infoch
            character(len=24) :: numedd
            character(len=19) :: solveu
            logical :: lostat
            character(len=24) :: time
            integer :: lonch
            character(len=24) :: matass
            character(len=19) :: maprec
            character(len=24) :: vtemp
            character(len=24) :: vtempm
            character(len=24) :: vtempp
            character(len=24) :: vhydr
            character(len=24) :: vhydrp
            character(len=24) :: tmpchi
            character(len=24) :: tmpchf
            character(len=24) :: compor
            character(len=24) :: cndirp
            character(len=24) :: cnchci
            character(len=24) :: vec2nd
            character(len=24) :: vec2ni
          end subroutine nxpred
        end interface

subroutine imbamo(nomres, ifm)
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.
!
! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.
!
! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!***********************************************************************
!    P. RICHARD     DATE 21/02/1991
!-----------------------------------------------------------------------
!  BUT:  IMPRIMER LES RESULTATS RELATIFS A LA BASE MODALE
    implicit none
!
!-----------------------------------------------------------------------
!
! NOMRES   /I/: NOM DU CONCEPT RESULTAT
! MAILLAGE /I/: NOM DU MAILLAGE
! IFM    /I/: UNITE DU FICHIER MESSAGE
!
!
!
!
    include 'jeveux.h'
!
!-----------------------------------------------------------------------
    include 'asterfort/dismoi.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/rsadpa.h'
    integer :: i, ibid, iret, nbdef, nbmod, nbpabm, nbtot
!
    real(kind=8) :: freq, genek, genem
!-----------------------------------------------------------------------
    parameter    (nbpabm=8)
    character(len=8) :: nomres, intf, nomnoe, nomcmp
    character(len=19) :: raid, mass, typeba
    character(len=14) :: numref
    integer :: ldpar(nbpabm), ifm, llref, ier
    character(len=16) :: bmpara(nbpabm), typdef
    character(len=8) :: rescyc
    character(len=8) :: k8bid
!
!
!-----------------------------------------------------------------------
!
    data  bmpara/&
     &  'NUME_MODE  '     , 'FREQ'       , 'NORME'           ,&
     &  'NOEUD_CMP'       , 'TYPE_DEFO'          , 'OMEGA2'   ,&
     &  'MASS_GENE'      , 'RIGI_GENE'/
!
!-----------------------------------------------------------------------
!
    call jemarq()
!
!----------------DETERMINATION DU TYPE DE LA BASE-----------------------
!
!------------------RECUPERATION DES CONCEPT AMONT-----------------------
!
    call jeveuo(nomres//'           .REFD', 'L', llref)
    raid=zk24(llref)
    mass=zk24(llref+1)
    numref=zk24(llref+3)
    intf=zk24(llref+4)
    typeba=zk24(llref+6)
!
!--------------------------------ECRITURES------------------------------
!
    write(ifm,*)' '
    write(ifm,*)'----------------------------------------------------'
    write(ifm,*)' '
    write(ifm,*)'                DEF_BASE_MODALE '
    write(ifm,*)' '
    write(ifm,*)'  IMPRESSIONS NIVEAU: 2'
    write(ifm,*)' '
    write(ifm,*) ' '
    write(ifm,*) ' NOM DE LA BASE MODALE: ',nomres
    write(ifm,*) '---------------------- '
!
    write(ifm,*) ' '
    write(ifm,*) ' '
!
!    CAS D'UNE BASE DE TYPE CONNUE
!
    if (typeba(1:9) .eq. 'CLASSIQUE') then
!
        call dismoi('F', 'NB_MODES_TOT', nomres, 'RESULTAT', nbtot,&
                    k8bid, ier)
        call dismoi('F', 'NB_MODES_STA', nomres, 'RESULTAT', nbdef,&
                    k8bid, ier)
        call dismoi('F', 'NB_MODES_DYN', nomres, 'RESULTAT', nbmod,&
                    k8bid, ier)
!
!
        write(ifm,*) '                TYPE BASE MODALE: CLASSIQUE'
        write(ifm,*) '                ----------------- '
        write(ifm,*) ' '
        write(ifm,*) '                INTERF_DYNA: ',intf
        write(ifm,*) '                NUMEROTATION: ',numref
        write(ifm,*) '                MATRICE RAIDEUR: ',raid
        write(ifm,*) '                MATRICE MASSE: ',mass
        write(ifm,*) '                NOMBRE DE MODE PROPRES: ',nbmod
        write(ifm,*) '                NOMBRE DE MODE STATIQUES: ',nbdef
!
!
    endif
!
!   CAS D'UNE BASE DE TYPE CYCLIQUE
!
    if (typeba(1:8) .eq. 'CYCLIQUE') then
        call dismoi('F', 'NB_MODES_TOT', nomres, 'RESULTAT', nbtot,&
                    k8bid, ier)
        call dismoi('F', 'NOM_MODE_CYCL', intf, 'INTERF_DYNA', ibid,&
                    rescyc, iret)
!
!
        write(ifm,*) '                TYPE BASE MODALE: CYCLIQUE'
        write(ifm,*) '                ----------------- '
        write(ifm,*) ' '
        write(ifm,*) '                INTERF_DYNA: ',intf
        write(ifm,*) '                NUMEROTATION: ',numref
!
!
    endif
!
! CAS D'UNE BASE DE RITZ
!
    if (typeba(1:4) .eq. 'RITZ') then
!
        call dismoi('F', 'NB_MODES_TOT', nomres, 'RESULTAT', nbtot,&
                    k8bid, ier)
!
        write(ifm,*) '                TYPE BASE MODALE: RITZ'
        write(ifm,*) '                ----------------- '
        write(ifm,*) ' '
        write(ifm,*) '                NUMEROTATION: ',numref
        write(ifm,*) '                DIMENSION BASE: ',nbtot
!
    endif
!
!
!
    write(ifm,*)' '
    write(ifm,*)'         DEFINITION DES DEFORMEES DE LA BASE MODALE'
    write(ifm,*)'         ------------------------------------------'
!
    do 10 i = 1, nbtot
!
        write(ifm,*)' '
        call rsadpa(nomres, 'L', nbpabm, bmpara, i,&
                    0, ldpar, k8bid)
!
        typdef=zk16(ldpar(5))
!
        if (typdef .eq. 'PROPRE') then
            freq=zr(ldpar(2))
            genek=zr(ldpar(8))
            genem=zr(ldpar(7))
            write(ifm,*)'NUME_ORDRE: ',i
            write(ifm,*)'              ','MODE PROPRE     FREQUENCE: ',&
     &                 freq,' HZ'
            write(ifm,*)'              ','MASS_GENE: ',genem,&
     &                ' RIGI_GENE: ',genek
!
        else
            nomnoe=zk16(ldpar(4))(1:8)
            nomcmp=zk16(ldpar(4))(9:16)
            write(ifm,*)'NUME_ORDRE: ',i
            write(ifm,*)'              ','MODE ',typdef
            write(ifm,*)'              ','NOEUD: ',nomnoe,&
     &                ' COMPOSANTE: ',nomcmp
!
        endif
10  end do
!
    call jedema()
end subroutine

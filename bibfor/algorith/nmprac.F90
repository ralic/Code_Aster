subroutine nmprac(fonact, lischa, numedd, numfix, solveu,&
                  sddyna, sdstat, sdtime, defico, resoco,&
                  meelem, measse, maprec, matass, faccvg)
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
    include 'jeveux.h'
!
    include 'asterfort/dismoi.h'
    include 'asterfort/infdbg.h'
    include 'asterfort/isfonc.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/mtdsc2.h'
    include 'asterfort/nmassm.h'
    include 'asterfort/nmchex.h'
    include 'asterfort/nmmatr.h'
    include 'asterfort/nmrinc.h'
    include 'asterfort/nmtime.h'
    include 'asterfort/preres.h'
    include 'asterfort/u2mess.h'
    integer :: fonact(*)
    character(len=19) :: sddyna, lischa
    character(len=24) :: sdstat, sdtime
    character(len=24) :: numedd, numfix
    character(len=19) :: solveu
    character(len=19) :: meelem(*), measse(*)
    character(len=24) :: defico, resoco
    character(len=19) :: maprec, matass
    integer :: faccvg
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (CALCUL - UTILITAIRE)
!
! CALCUL DE LA MATRICE GLOBALE ACCELERATION INITIALE
!
! ----------------------------------------------------------------------
!
!
! IN  NUMEDD : NUME_DDL (VARIABLE AU COURS DU CALCUL)
! IN  NUMFIX : NUME_DDL (FIXE AU COURS DU CALCUL)
! IN  LISCHA : LISTE DES CHARGES
! IN  RESOCO : SD RESOLUTION CONTACT
! IN  DEFICO : SD DEFINITION CONTACT
! IN  SDTIME : SD TIMER
! IN  SDSTAT : SD STATISTIQUES
! IN  SDDYNA : SD POUR LA DYNAMIQUE
! IN  SOLVEU : SOLVEUR
! IN  MEELEM : VARIABLE CHAPEAU POUR NOM DES MATR_ELEM
! IN  MEASSE : VARIABLE CHAPEAU POUR NOM DES MATR_ASSE
! OUT MATASS : MATRICE DE RESOLUTION ASSEMBLEE
! OUT MAPREC : MATRICE DE RESOLUTION ASSEMBLEE - PRECONDITIONNEMENT
! OUT FACCVG : CODE RETOUR (INDIQUE SI LA MATRICE EST SINGULIERE)
!                   O -> MATRICE INVERSIBLE
!                   1 -> MATRICE SINGULIERE
!                   2 -> MATRICE PRESQUE SINGULIERE
!                   3 -> ON NE SAIT PAS SI LA MATRICE EST SINGULIERE
!
! ----------------------------------------------------------------------
!
    logical :: lctcc
    integer :: ieq, iret, ibid, numins
    integer :: iadia, neq, lres, neql
    character(len=8) :: k8bid, kmatd
    integer :: jvalm, islvi, zislv1, zislv3
    integer :: ifm, niv
    character(len=16) :: optass
    character(len=19) :: masse
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('MECA_NON_LINE', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> ...... CALCUL MATRICE'
    endif
!
! --- INITIALISATIONS
!
    faccvg = -1
    numins = 1
    call dismoi('F', 'NB_EQUA', numedd, 'NUME_DDL', neq,&
                k8bid, iret)
!
! --- FONCTIONNALITES ACTIVEES
!
    lctcc = isfonc(fonact,'CONT_CONTINU')
!
! --- DECOMPACTION DES VARIABLES CHAPEAUX
!
    call nmchex(measse, 'MEASSE', 'MEMASS', masse)
!
! --- ASSEMBLAGE DE LA MATRICE MASSE
!
    optass = 'AVEC_DIRICHLET'
    call nmassm(fonact, lischa, solveu, numedd, numfix,&
                'MEMASS', optass, meelem, masse)
!
! --- CALCUL DE LA MATRICE ASSEMBLEE GLOBALE
!
    call nmmatr('ACCEL_INIT', fonact, lischa, solveu, numedd,&
                sddyna, numins, defico, resoco, meelem,&
                measse, matass)
!
! --- SI METHODE CONTINUE ON REMPLACE LES TERMES DIAGONAUX NULS PAR
! --- DES UNS POUR POUVOIR INVERSER LA MATRICE ASSEMBLE MATASS
!
    if (lctcc) then
        call mtdsc2(matass, 'SXDI', 'L', iadia)
        call dismoi('F', 'MATR_DISTR', matass, 'MATR_ASSE', ibid,&
                    kmatd, ibid)
        if (kmatd .eq. 'OUI') then
            call jeveuo(matass//'.&INT', 'L', lres)
            neql = zi(lres+5)
        else
            neql = neq
        endif
        call jeveuo(jexnum(matass//'.VALM', 1), 'E', jvalm)
        do 10 ieq = 1, neql
            if (zr(jvalm-1+zi(iadia-1+ieq)) .eq. 0.d0) then
                zr(jvalm-1+zi(iadia-1+ieq)) = 1.d0
            endif
10      continue
    endif
!
! --- ON ACTIVE LA DETECTION DE SINGULARITE (NPREC=8)
! --- ON EVITE L'ARRET FATAL LORS DE L'INVERSION DE LA MATRICE
!
    call jeveuo(solveu//'.SLVI', 'E', islvi)
    zislv1 = zi(islvi-1+1)
    zislv3 = zi(islvi-1+3)
    zi(islvi-1+1) = 8
    zi(islvi-1+3) = 2
!
! --- FACTORISATION DE LA MATRICE ASSEMBLEE GLOBALE
!
    call nmtime(sdtime, 'INI', 'FACTOR')
    call nmtime(sdtime, 'RUN', 'FACTOR')
    call preres(solveu, 'V', faccvg, maprec, matass,&
                ibid, -9999)
    call nmtime(sdtime, 'END', 'FACTOR')
    call nmrinc(sdstat, 'FACTOR')
!
! --- RETABLISSEMENT CODE
!
    zi(islvi-1+1) = zislv1
    zi(islvi-1+3) = zislv3
!
! --- LA MATRICE PEUT ETRE QUASI-SINGULIERE PAR EXEMPLE POUR LES DKT
!
    if (faccvg .eq. 1) then
        call u2mess('A', 'MECANONLINE_78')
    endif
!
    call jedema()
!
end subroutine

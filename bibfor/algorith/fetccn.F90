subroutine fetccn(chamn1, chamn2, chamn3, chamn4, typcum,&
                  chamnr)
!-----------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!-----------------------------------------------------------------------
!    - FONCTION REALISEE:  CONCATENATION DE CHAM_NOS EN UN SEUL. EN
!      FAIT UNIQUEMENT DE LEUR .VALE. LEURS .REFE/.DESC SONT SUPPOSES
!      IDENTIQUES ET LES CHAM_NOS HOMOGENES (TOUS FETI OU AUCUN, ET SI
!      FETI AVEC LA MEME SD_FETI).
!      POUR GAGNER DU TEMPS AUCUN TEST D'HOMOGENEITE N'EST EFFECTUE. ILS
!      SONT NORMALEMENT FAITS (ET REFAITS !) EN AMONT.
!
! IN CHAMN1/4 : CHAM_NOS A CONCATENER
! IN TYPCUM   : TYPE DE CUMUL
! OUT CHAMNR  : CHAM_NO RESULTAT
!----------------------------------------------------------------------
! person_in_charge: olivier.boiteau at edf.fr
! CORPS DU PROGRAMME
    implicit none
!
! DECLARATION PARAMETRES D'APPELS
    include 'jeveux.h'
!
    include 'asterfort/assert.h'
    include 'asterfort/infniv.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/utimsd.h'
    integer :: typcum
    character(len=19) :: chamn1, chamn2, chamn3, chamn4, chamnr
!
!
! DECLARATION VARIABLES LOCALES
    integer :: idd, iret, nbsd, i1, i2, i3, i4, ir, nbval, k, j1, j2, j3, j4, jr
    integer :: idd1, ifm, niv, iinf, ilimpi
    character(len=5) :: vale, fetc
    character(len=8) :: k8bid
    character(len=19) :: cham1b, cham2b, cham3b, cham4b, chamrb
    character(len=24) :: k24b, infofe
    logical :: lfeti, iddok
!
! CORPS DU PROGRAMME
    call jemarq()
! RECUPERATION ET MAJ DU NIVEAU D'IMPRESSION
    call infniv(ifm, niv)
!
! INIT.
    vale='.VALE'
    fetc='.FETC'
!
! FETI OR NOT ?
    k24b=chamnr//fetc
    call jeexin(k24b, iret)
    if (iret .gt. 0) then
        lfeti=.true.
! NBRE DE SOUS-DOMAINES NBSD
        call jelira(k24b, 'LONMAX', nbsd, k8bid)
!
! PREPARATION POUR LA BOUCLE SUR LES SOUS-DOMAINES. STOCKAGE
! DES ADRESSES DES .FETC DE CHACUN DES CHAM_NOS A CONCATENER
        if (typcum .gt. 0) call jeveuo(chamn1//fetc, 'L', i1)
        if (typcum .gt. 1) call jeveuo(chamn2//fetc, 'L', i2)
        if (typcum .gt. 2) call jeveuo(chamn3//fetc, 'L', i3)
        if (typcum .gt. 3) call jeveuo(chamn4//fetc, 'L', i4)
        call jeveuo(chamnr//fetc, 'L', ir)
        call jeveuo('&FETI.FINF', 'L', iinf)
        infofe=zk24(iinf)
        call jeveuo('&FETI.LISTE.SD.MPI', 'L', ilimpi)
    else
        lfeti=.false.
        infofe=' '
        nbsd=0
    endif
!
!========================================
! BOUCLE SUR LES SOUS-DOMAINES + IF MPI:
!========================================
! IDD=0 --> DOMAINE GLOBAL/ IDD=I --> IEME SOUS-DOMAINE
    do 10 idd = 0, nbsd
!
! TRAVAIL PREALABLE POUR DETERMINER SI ON EFFECTUE LA BOUCLE SUIVANT
! LE SOLVEUR (FETI OU NON), LE TYPE DE RESOLUTION (PARALLELE OU
! SEQUENTIELLE) ET L'ADEQUATION "RANG DU PROCESSEUR-NUMERO DU SD"
        if (.not.lfeti) then
            iddok=.true.
        else
            if (zi(ilimpi+idd) .eq. 1) then
                iddok=.true.
            else
                iddok=.false.
            endif
        endif
        if (iddok) then
!
            if (lfeti) call jemarq()
            if (idd .eq. 0) then
! DOMAINE GLOBAL
                cham1b=chamn1
                cham2b=chamn2
                cham3b=chamn3
                cham4b=chamn4
                chamrb=chamnr
            else
! SOUS-DOMAINE N°IDD
                idd1=idd-1
                if (typcum .gt. 0) cham1b=zk24(i1+idd1)
                if (typcum .gt. 1) cham2b=zk24(i2+idd1)
                if (typcum .gt. 2) cham3b=zk24(i3+idd1)
                if (typcum .gt. 3) cham4b=zk24(i4+idd1)
                chamrb=zk24(ir+idd1)
            endif
            if (typcum .gt. 0) call jeveuo(cham1b//vale, 'L', j1)
            if (typcum .gt. 1) call jeveuo(cham2b//vale, 'L', j2)
            if (typcum .gt. 2) call jeveuo(cham3b//vale, 'L', j3)
            if (typcum .gt. 3) call jeveuo(cham4b//vale, 'L', j4)
            call jeveuo(chamrb//vale, 'E', jr)
            call jelira(chamrb//vale, 'LONMAX', nbval, k8bid)
            nbval=nbval-1
!
!----------------------------------------------------------------------
! ----- BOUCLE SUR LES .VALE SELON LES TYPES DE CUMUL
!----------------------------------------------------------------------
!
            if (typcum .eq. 0) then
                do 2 k = 0, nbval
                    zr(jr+k) = 0.d0
 2              continue
            else if (typcum.eq.1) then
                do 3 k = 0, nbval
                    zr(jr+k) = zr(j1+k)
 3              continue
            else if (typcum.eq.2) then
                do 4 k = 0, nbval
                    zr(jr+k) = zr(j1+k) + zr(j2+k)
 4              continue
            else if (typcum.eq.3) then
                do 5 k = 0, nbval
                    zr(jr+k) = zr(j1+k) + zr(j2+k) + zr(j3+k)
 5              continue
            else if (typcum.eq.4) then
                do 6 k = 0, nbval
                    zr(jr+k) = zr(j1+k) + zr(j2+k) + zr(j3+k) + zr(j4+ k)
 6              continue
            else
                call assert(.false.)
            endif
! MONITORING
            if ((infofe(1:1).eq.'T') .and. (nbsd.gt.0)) then
                if (idd .eq. 0) then
                    write(ifm,*)'<FETI/FETCCN> DOMAINE GLOBAL ',&
                    chamrb(1:19)
                else
                    write(ifm,*)'<FETI/FETCCN> SD: ',idd,' ',chamrb(1:&
                    19)
                endif
            endif
            if ((infofe(2:2).eq.'T') .and. (idd.ne.0)) call utimsd(ifm, 2, .false., .true.,&
                                                                   chamrb(1:19), 1, ' ')
            if ((infofe(2:2).eq.'T') .and. (idd.eq.nbsd)) call utimsd(ifm, 2, .false., .true.,&
                                                                      chamnr(1:19), 1, ' ')
            if (lfeti) call jedema()
        endif
10  end do
!========================================
! FIN BOUCLE SUR LES SOUS-DOMAINES + IF MPI:
!========================================
    call jedema()
end subroutine

subroutine nmdecv(sddisc, numins, ievdac, dtmin, retdec)
!
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit     none
    include 'jeveux.h'
    include 'asterc/r8prem.h'
    include 'asterfort/dinins.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/u2mesi.h'
    include 'asterfort/u2mesr.h'
    include 'asterfort/utdidt.h'
    character(len=19) :: sddisc
    integer :: numins, ievdac, retdec
    real(kind=8) :: dtmin
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (GESTION DES EVENEMENTS - DECOUPE)
!
! VERIFICATIONS DE LA DECOUPE
!
! ----------------------------------------------------------------------
!
!
! IN  SDDISC : SD DISCRETISATION TEMPORELLE
! IN  NUMINS : NUMERO DE L'INSTANT COURANT
! IN  IEVDAC : INDICE DE L'EVENEMENT ACTIF
! IN  DTMIN  : INTERVALLE DE TEMPS MINIMAL SUR LA LISTE CREEE
! OUT RETDEC : CODE RETOUR DECOUPE
!               0 ECHEC DE LA DECOUPE
!               1 ON A DECOUPE
!               2 PAS DE DECOUPE
!
! ----------------------------------------------------------------------
!
    real(kind=8) :: r8bid
    character(len=8) :: k8bid
    integer :: ibid
    integer :: nbnivo, lenivo
    real(kind=8) :: pasmin
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- NIVEAU DE REDECOUPAGE ACTUEL
!
    lenivo = dinins(sddisc,numins)
!
! --- NIVEAU MAXI DE SUBDIVISION
!
    call utdidt('L', sddisc, 'ECHE', ievdac, 'SUBD_NIVEAU',&
                r8bid, nbnivo, k8bid)
!
! --- PAS MINIMUM
!
    call utdidt('L', sddisc, 'ECHE', ievdac, 'SUBD_PAS_MINI',&
                pasmin, ibid, k8bid)
!
! --- TAILLE DE PAS MINIMALE ATTEINTE PENDANT LA SUBDIVISION
!
    if ((dtmin .lt. pasmin) .or. (dtmin.le.r8prem())) then
        retdec = 0
        call u2mesr('I', 'SUBDIVISE_16', 1, pasmin)
        goto 999
    else
        retdec = 1
    endif
!
! --- NIVEAU MAXIMUM DE REDECOUPAGE ATTEINT
!
    if (( nbnivo .gt. 1 ) .and. (lenivo.eq.nbnivo)) then
        call u2mesi('I', 'SUBDIVISE_17', 1, lenivo)
        retdec = 0
    else
        retdec = 1
    endif
!
999  continue
!
    call jedema()
end subroutine

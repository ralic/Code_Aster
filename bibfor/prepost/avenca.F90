subroutine avenca(rvecpg, nbvec, nbordr, lsig0, iflag,&
                  rmima)
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
! person_in_charge: jean.angles at edf.fr
    implicit none
    include 'jeveux.h'
!
    include 'asterc/r8maem.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    integer :: nbvec, nbordr, iflag(nbvec)
    real(kind=8) :: rvecpg(2*nbvec*nbordr), rmima(4*nbvec)
    logical :: lsig0
! ----------------------------------------------------------------------
! BUT: ENCADRER LES POINTS REPRESANTANT LE CISAILLEMENT TAU
!      DANS LE PLAN DE CISAILLEMENT (PLAN u, v).
! ----------------------------------------------------------------------
! ARGUMENTS:
! RVECPG    IN   R  : VECTEUR DE TRAVAIL CONTENANT LES
!                     COMPOSANTES u ET v DU VECTEUR TAU (CISAILLEMENT),
!                     POUR TOUS LES VECTEURS NORMAUX (n) ET TOUS LES
!                     NUMEROS D'ORDRE.
!                     VECTEUR NORMAL ASSOCIE A DELTA_TAU_MAX.
! NBVEC     IN   I  : NOMBRE DE VECTEURS NORMAUX.
! NBORDR    IN   I  : NOMBRE DE NUMERO D'ORDRE STOCKE DANS LA
!                     STRUCTURE DE DONNEES RESULTAT.
! LSIG0     OUT  L  : VARIABLE LOGIQUE QUI INDIQUE :
!                      - LSIG0 = FALSE --> CAS GENERAL, LES CONTRAINTES
!                                          SONT DIFFERENTES DE ZERO ;
!                      - LSIG0 =  TRUE --> LES CONTRAINTES SONT NULLES
!                                          A TOUS LES PAS DE TEMPS, QUEL
!                                          QUE SOIT LE VECTEUR NORMAL.
! IFLAG     OUT  I  : VECTEUR DE DRAPEAUX QUI INDIQUE :
!                      - IFLAG(i) = 0 --> CAS GENERAL ;
!                      - IFLAG(i) = 1 --> CAS OU LES POINTS DANS LE
!                                         PLAN DE CISAILLEMENT SONT
!                                         ALIGNES VERTICALEMENT ;
!                      - IFLAG(i) = 2 --> CAS OU LES POINTS DANS LE
!                                         PLAN DE CISAILLEMENT SONT
!                                         ALIGNES HORIZONTALEMENT ;
!                      - IFLAG(i) = 3 --> CAS OU LES POINTS DANS LE
!                                         PLAN DE CISAILLEMENT SONT
!                                         CONTENUS DANS UN CADRE DE
!                                         COTES INFERIEURS A EPSILO.
! RMIMA     OUT  R  : VECTEUR CONTENANT LES COORDONNEES DES POINTS
!                     EXTREMES DU CADRE (CUMIN, CUMAX, CVMIN, CVMAX)
!                     POUR TOUS LES VECTEURS NORMAUX.
!
!-----------------------------------------------------------------------
!     ------------------------------------------------------------------
    integer :: n1, ivect, iordr, nsig0
!
    real(kind=8) :: epsilo, cumin, cumax, cvmin, cvmax
    real(kind=8) :: cui, cvi
!
!-----------------------------------------------------------------------
!234567                                                              012
!
    call jemarq()
!
!-----------------------------------------------------------------------
!     ------------------------------
!    |  TRAITEMENT DU CAS GENERAL  |
!    ------------------------------
!-----------------------------------------------------------------------
!
    epsilo = 1.0d-5
!
! ININTIALISATION
!
    n1 = 0
    nsig0 = 0
!
    do 30 ivect = 1, nbvec
!
! INITIALISATION
!
        iflag(ivect) = 0
!
        cumin = r8maem()
        cumax = -r8maem()
        cvmin = r8maem()
        cvmax = -r8maem()
!
        do 40 iordr = 1, nbordr
            n1 = n1 + 1
            cui = rvecpg(2*n1 -1)
            cvi = rvecpg(2*n1)
!
            if (cui .lt. cumin) then
                cumin = cui
            endif
            if (cui .gt. cumax) then
                cumax = cui
            endif
            if (cvi .lt. cvmin) then
                cvmin = cvi
            endif
            if (cvi .gt. cvmax) then
                cvmax = cvi
            endif
40      continue
!
!-----------------------------------------------------------------------
!   ------------------------------------
!  |  TRAITEMENT DES CAS PARTICULIERS  |
!  ------------------------------------
!-----------------------------------------------------------------------
!
! 1/ CAS OU TOUS LES POINTS SONT ALIGNES VERTICALEMENT, ON NE FERA PAS
!    DE PROJECTION.
!
        if (abs(cumax-cumin)/2.d0 .lt. epsilo) then
            iflag(ivect) = 1
!
! 2/ CAS OU TOUS LES POINTS SONT ALIGNES HORIZONTALEMENT, ON NE FERA
!    PAS DE PROJECTION.
!
        else if (abs(cvmax-cvmin)/2.d0 .lt. epsilo) then
            iflag(ivect) = 2
!
! 3/ CAS OU TOUS LES POINTS SONT DANS UNE BOITE DONT LES DEUX COTES
!    SONT INFERIEURS A EPSILO, ON NE FERA PAS DE PROJECTION.
!
            elseif ( (abs(cvmax-cvmin)/2.d0 .lt. epsilo) .and. (abs(cumax-&
        cumin)/2.d0 .lt. epsilo) ) then
            iflag(ivect) = 3
            nsig0 = nsig0 + 1
        endif
!
        rmima(4*ivect - 3) = cumin
        rmima(4*ivect - 2) = cumax
        rmima(4*ivect - 1) = cvmin
        rmima(4*ivect) = cvmax
!
        if (nsig0 .eq. nbvec) then
            lsig0 = .true.
        endif
!
30  end do
!
    call jedema()
end subroutine

subroutine memver(action, prec, arret, titre)
    implicit none
    include 'asterfort/assert.h'
    include 'asterfort/memres.h'
    include 'asterfort/utgtme.h'
    character(len=*) :: action, arret, titre
    real(kind=8) :: prec, precs
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
! person_in_charge: jacques.pellet at edf.fr
!   BUT: VERIFIER QU'IL N'Y A PAS DE FUITE MEMOIRE DANS UN
!        PROGRAMME FORTRAN
!---------------------------------------------------------------------
! IN  ACTION : /'MESURE' : ON MESURE LA MEMOIRE TOTALE =
!              MEMOIRE LIBRE + MEMOIRE JEVEUX DYNAMIQUE ALLOUEE
!              /'VERIF' : ON VERIFIE QUE LA MEMOIRE TOTALE N'A PAS
!              BOUGE DEPUIS L'APPEL A "MESURE"
! IN  PREC : PRECISION SOUHAITEE POUR LA COMPARAISON.
!            PREC EST DONNE POUR 'MESURE'. IL EST STOCKE.
!            ET IL EST UTILISE POUR ACTION='VERIF'
! IN  ARRET  (K2)
!   ARRET(1:1) :
!     /'F' : ERREUR FATALE SI FUITE MEMOIRE > PREC
!     /' ' : NE S'ARRETE PAS EN CAS DE FUITE
!   ARRET(2:2) :
!     /'I' : IMPRIME LES 2 VALEURS COMPAREES AINSI QUE LEUR DIFFERENCE
!     /' ' : N'IMPRIME RIEN
! IN  TITRE : CHAINE DE CARACTERES IMPRIMEE AU DEBUT DES LIGNES
!            SI ARRET(2:2)='I'
!----------------------------------------------------------------------
!
    real(kind=8) :: tmax, mtots, mtot, rval(1)
    character(len=8) :: k8tab(1)
    integer :: iret
    save mtots,precs
!
    call assert(action.eq.'MESURE' .or. action.eq.'VERIF')
    if (action .eq. 'MESURE') precs=2.d0*prec
!
    call assert(arret(1:1).eq.'F' .or. arret(1:1).eq.' ')
    call assert(arret(2:2).eq.'I' .or. arret(2:2).eq.' ')
!
    call memres('NON', 'NON', ' ', precs, tmax)
    k8tab(1) = 'COUR_JV'
    call utgtme(1, k8tab, rval, iret)
    mtot=tmax+rval(1)
!
    if (action .eq. 'MESURE') then
        mtots=mtot
    else
        if (arret(2:2) .eq. 'I') then
            write (6,9000)'<MEMVER> MTOTS,MTOT,DIFF=',titre, mtots,&
            mtot,mtots-mtot
        endif
        if (arret(1:1) .eq. 'F') call assert(mtots-mtot.lt.precs)
    endif
!
    9000 format (2(a,1x),3(f15.3,1x))
end subroutine

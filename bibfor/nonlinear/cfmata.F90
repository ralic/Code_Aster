subroutine cfmata(resoco, neq, nbliai, nmult, numedz,&
                  matelz, numecz, matriz)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
    implicit      none
#include "jeveux.h"
#include "asterfort/atasmo.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    character(len=24) :: resoco
    character(len=*) :: numedz, numecz, matriz, matelz
    integer :: neq, nbliai, nmult
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (RESOLUTION - UTILITAIRE)
!
! CALCUL DE LA MATRICE AT*A
!
! ----------------------------------------------------------------------
!
!
! IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
! IN  NBLIAI : NOMBRE DE LIAISONS DE CONTACT
! IN  NEQ    : NOMBRE D'EQUATIONS
! IN  NUMEDD : NOM DU NUME_DDL GLOBAL
! IN  NUMECF : NOM DU NUME_DDL A CREER POUR LA MATRICE
! IN  MATELE : NOM DE LA COLLECTION DES VECTEURS
!              LIGNES (I.E. MATRICE RECTANGULAIRE POUR LAQUELLE ON VA
!                      CALCULER LE PRODUIT).
! OUT MATRIX : MATRICE RESULTANTE
!
!
!
!
    character(len=24) :: appoin, apddl
    integer :: japptr, japddl
    character(len=14) :: numedd, numecf
    character(len=19) :: matrix
    character(len=24) :: matele
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    numedd = numedz
    numecf = numecz
    matrix = matriz
    matele = matelz
!
! --- LECTURE DES STRUCTURES DE DONNEES DE CONTACT
!
    appoin = resoco(1:14)//'.APPOIN'
    apddl = resoco(1:14)//'.APDDL'
    call jeveuo(appoin, 'L', japptr)
    call jeveuo(apddl, 'L', japddl)
!
! --- CONSTRUCTION NOUVELLE MATRICE
!
    call atasmo(neq, matele, zi(japddl), zi(japptr), numedd,&
                matrix, 'V', nbliai, nmult, numecf)
!
    call jedema()
!
end subroutine

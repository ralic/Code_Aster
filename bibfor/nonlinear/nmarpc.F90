subroutine nmarpc(result, sdener, numrep, instan)
!
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
    implicit     none
#include "jeveux.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/ltnotb.h"
#include "asterfort/tbajli.h"
    real(kind=8) :: instan
    character(len=8) :: result
    character(len=19) :: sdener
    integer :: numrep
!
! ----------------------------------------------------------------------
!
! ROUTINE *_NON_LINE (STRUCTURES DE DONNES)
!
! ARCHIVAGE DES PARAMETRES DANS LA TABLE DES PARAMETRES CALCULES
!
! ----------------------------------------------------------------------
!
! IN  RESULT : NOM SD RESULTAT
! IN  SDENER : NOM SD ENERGIE
! IN  INSTAN : VALEUR DE L'INSTANT DE CALCUL
! IN  NUMREP : NUMERO DE REUSE POUR LA TABLE PARA_CALC
!
! ----------------------------------------------------------------------
!
    integer :: nbpar
    parameter   (nbpar=8)
    character(len=10) :: nompar(nbpar)
! ----------------------------------------------------------------------
    integer :: ifm, niv
    integer :: jener, iparar
    character(len=19) :: tablpc
    integer :: vali
    character(len=8) :: k8bid
    complex(kind=8) :: c16bid
    real(kind=8) :: valr(7)
!
    data         nompar / 'NUME_REUSE','INST'      ,'TRAV_EXT  ',&
     &                      'ENER_CIN'  ,'ENER_TOT'  ,'TRAV_AMOR ',&
     &                      'TRAV_LIAI' ,'DISS_SCH'/
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('MECA_NON_LINE', ifm, niv)
!
! --- RECUPERATION DU NOM DE LA TABLE CORRESPONDANT
!     AUX PARAMETRE CALCULES
!
    call ltnotb(result, 'PARA_CALC', tablpc)
!
! --- CONSTRUCTION DES LISTES DES PARAMETRES
!
    call jeveuo(sdener//'.VALE', 'L', jener)
    valr(1) = instan
    do 10 iparar = 1, 6
        valr(1+iparar) = zr(jener-1+iparar)
10  end do
    vali = numrep
!
! --- CONSTRUCTION DES LISTES DE PARAMETRES A SAUVEGARDER PAR TYPE
!
    call tbajli(tablpc, nbpar, nompar, [vali], valr,&
                [c16bid], k8bid, 0)
!
    call jedema()
!
end subroutine

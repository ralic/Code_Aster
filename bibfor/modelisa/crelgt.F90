subroutine crelgt(basez, ligrez)
    implicit none
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
!
!
! ======================================================================
! ------------------------------------------------------------
!     BUT : CREATION DE L'OBJET DE TYPE LIGRET
!           ET DE NOM LIGRET
!           LE NOM LIGRET EST FOURNI EN ARGUMENT
!           SI L'OBJET LIGRET  EXISTE DEJA, ON LE DETRUIT
!           PUIS ON LE RECREE
!
!           LE NOMBRE D'AFFECTATION DU LIGRET EST DIMENSIONNE A
!           NBAJEL = 1000
!
!           LE NOMBRE DE MAILLES DU LIGRET EST DIMENSIONNE A
!           NBMAIL = 10000
!
!  ARGUMENT       E/S    TYPE          ROLE
!  BASEZ          IN      K1      BASE SUR LAQUELLE EST CREE LE LIGRET
!
!  LIGREZ         IN      K19     NOM DU LIGRET
!                 JXOUT
!-------------------------------------------------------------
!
! ====================== DEBUT DES DECLARATIONS ========================
!
! ----- ARGUMENTS
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/wkvect.h"
    character(len=*) :: basez, ligrez
! ----- VARIABLES LOCALES -------------------------------
!-----------------------------------------------------------------------
    integer :: idlima, idlino, idlity, idmata, idmode, idnbma, idpama
    integer :: idpano, idphen, idpoma, idpono, iret, jlgrf, nbajel
    integer :: nbmail
!-----------------------------------------------------------------------
    parameter (nbajel = 1000 )
    parameter (nbmail = 10000)
    character(len=1) :: base
    character(len=19) :: ligret
! ====================== DEBUT DU CODE EXECUTABLE ======================
!
    call jemarq()
!
! --- INITIALISATIONS :
!     ---------------
    base = basez
    ligret = ligrez
!
! --- SI L'OBJET LIGRET EXISTE , ON LE DETRUIT :
!     ----------------------------------------
    call jeexin(ligret//'.LGRF', iret)
!
    if (iret .ne. 0) then
        call jedetr(ligret//'.LIMA')
        call jedetr(ligret//'.LITY')
        call jedetr(ligret//'.MODE')
        call jedetr(ligret//'.PHEN')
        call jedetr(ligret//'.POMA')
        call jedetr(ligret//'.PONO')
        call jedetr(ligret//'.LGRF')
        call jedetr(ligret//'.MATA')
        call jedetr(ligret//'.LINO')
        call jedetr(ligret//'.APMA')
        call jedetr(ligret//'.APNO')
        call jedetr(ligret//'.NBMA')
    endif
!
! ---  CREATION DU VECTEUR DE LA LISTE DES MAILLES CUMULEES DU LIGRET :
!      --------------------------------------------------------------
    call wkvect(ligret//'.LIMA', base//' V I', nbmail, idlima)
    call jeecra(ligret//'.LIMA', 'LONUTI', 0)
!
! ---  CREATION DU VECTEUR DES TYPES DES MAILLES CUMULEES DU LIGRET :
!      ------------------------------------------------------------
    call wkvect(ligret//'.LITY', base//' V I', nbmail, idlity)
!
! ---  CREATION DU K16 QUI VA CONTENIR LE NOM DE LA MODELISATION :
!      ---------------------------------------------------------
    call wkvect(ligret//'.MODE', base//' V K16', 1, idmode)
!
! ---  CREATION DU K16 QUI VA CONTENIR LE NOM DU PHENOMENE :
!      ---------------------------------------------------
    call wkvect(ligret//'.PHEN', base//' V K16', 1, idphen)
!
! ---  CREATION DU TABLEAU DE POINTEURS DANS LA LISTE DES MAILLES :
!      ----------------------------------------------------------
    call wkvect(ligret//'.POMA', base//' V I', nbajel+1, idpoma)
!
! ---  CREATION DU TABLEAU DE POINTEURS DANS LA LISTE DES NOEUDS :
!      ---------------------------------------------------------
    call wkvect(ligret//'.PONO', base//' V I', nbajel+1, idpono)
!
! ---  CREATION DU K8 QUI EST LE NOM DU MAILLAGE :
!      -----------------------------------------
    call wkvect(ligret//'.LGRF', base//' V K8', 2, jlgrf)
!
! ---  CREATION DE L'ENTIER QUI EST LE NOMBRE DE MAILLES TARDIVES :
!      ----------------------------------------------------------
    call wkvect(ligret//'.MATA', base//' V I', 1, idmata)
!
! ---  CREATION DU VECTEUR DE LA LISTE DES NOEUDS CUMULES DU LIGRET :
!      ------------------------------------------------------------
    call wkvect(ligret//'.LINO', base//' V I', nbmail, idlino)
    call jeecra(ligret//'.LINO', 'LONUTI', 0)
!
! ---  CREATION DE L'ENTIER QUI EST LE NOMBRE D'AFFECTATIONS DE
! ---  MAILLES (VIA AJELLT) :
!      --------------------
    call wkvect(ligret//'.APMA', base//' V I', 1, idpama)
!
! ---  CREATION DE L'ENTIER QUI EST LE NOMBRE D'AFFECTATIONS DE
! ---  NOEUDS (VIA AJELLT) :
!      -------------------
    call wkvect(ligret//'.APNO', base//' V I', 1, idpano)
!
! ---  CREATION DE L'ENTIER QUI EST LE NOMBRE DE MAILLES PHYSIQUES :
!      -----------------------------------------------------------
    call wkvect(ligret//'.NBMA', base//' V I', 1, idnbma)
!
    call jedema()
end subroutine

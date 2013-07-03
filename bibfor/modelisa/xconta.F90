subroutine xconta(char, noma, nomo, ndim)
!
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
!
    implicit none
#include "jeveux.h"
#include "asterfort/celces.h"
#include "asterfort/cescel.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/exixfe.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mminfi.h"
#include "asterfort/u2mesi.h"
#include "asterfort/u2mesk.h"
#include "asterfort/wkvect.h"
#include "asterfort/xbarvi.h"
#include "asterfort/xdefco.h"
#include "asterfort/xxconi.h"
    character(len=8) :: char
    integer :: ndim
    character(len=8) :: noma, nomo
!
! ----------------------------------------------------------------------
!
! ROUTINE XFEM (MODIF. DU MODELE)
!
! PREPARATION DONNEES RELATIVES AU CONTACT POUR LIAISONS LINEAIRES
!
! ----------------------------------------------------------------------
!
!
! IN  CHAR   : NOM UTILISATEUR DU CONCEPT DE CHARGE
! IN  NOMA   : NOM DU MAILLAGE
! IN  NOMO   : NOM DU MODELE
! IN  NDIM   : DIMENSION DE L'ESPACE
!
!
!
!
    integer :: nfismx
    parameter    (nfismx=100)
!
    integer :: ifiss, algola, izone
    integer :: nfiss, ier
    integer :: jfiss, jnfis, nbma, ibid, jnbpt
    character(len=8) :: fiscou, k8bid
    character(len=24) :: defico
    character(len=16) :: valk(2)
    character(len=24) :: xnrell
    integer :: jxnrel
    character(len=19) :: nliseq, faclon, ainter
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    defico = char(1:8)//'.CONTACT'
    faclon = '&&XCONTA.FACLON'
    ainter = '&&XCONTA.AINTER'
!
! --- ACCES A LA SD FISSURE
!
    call exixfe(nomo, ier)
    if (ier .eq. 0) then
        valk(1) = nomo
        call u2mesk('F', 'XFEM2_8', 1, valk)
    endif
    call jeveuo(nomo(1:8)//'.FISS', 'L', jfiss)
    call jeveuo(nomo(1:8)//'.NFIS', 'L', jnfis)
    nfiss = zi(jnfis)
    if (nfiss .gt. nfismx) then
        call u2mesi('F', 'XFEM_2', 1, nfismx)
    endif
!
! --- CREATION SD POUR SD REL. LIN.
!
    xnrell = defico(1:16)//'.XNRELL'
    call wkvect(xnrell, 'G V K24', nfiss, jxnrel)
!
! --- CHAMP TEMPORAIRE STOCKANT LE COMPTAGE DES FISS VUES EN MULTI-HEAV
!
    call dismoi('F', 'NB_MA_MAILLA', noma, 'MAILLAGE', nbma,&
                k8bid, ibid)
    call wkvect('&&XCONTA.NBSP', 'V V I', nbma, jnbpt)
!
! --- TRANSFO CHAM_ELEM -> CHAM_ELEM_S
!
    call celces(nomo//'.TOPOFAC.LO', 'V', faclon)
    call celces(nomo//'.TOPOFAC.AI', 'V', ainter)
!
! --- CHOIX DE L'ESPACE DES LAGRANGES POUR LE CONTACT
!
    do 220 ifiss = 1, nfiss
!
! --- FISSURE COURANTE
!
        fiscou = zk8(jfiss+ifiss-1)
!
! --- NOM DES SD POUR REL. LIN.
!
        nliseq = fiscou(1:8)//'.LISEQ     '
        zk24(jxnrel+ifiss-1) = nliseq
!
!
! --- ZONE DE CONTACT IZONE CORRESPONDANTE
!
        izone = xxconi(defico,fiscou,'MAIT')
!
! --- TYPE LIAISON POUR CONTACT
!
        algola = mminfi(defico,'XFEM_ALGO_LAGR',izone )
!
! --- CHOIX DE L'ESPACE DES LAGRANGES POUR LE CONTACT
!
        call xdefco(noma, nomo, fiscou, algola, ndim,&
                    nliseq)
!
        call xbarvi(noma, nomo, fiscou, faclon, ainter)
220  end do
!
! --- CONVERSION CHAM_ELEM_S -> CHAM_ELEM
!
    call cescel(ainter, nomo//'.MODELE', 'TOPOFA', 'PAINTER', 'OUI',&
                ibid, 'G', nomo//'.TOPOFAC.AI', 'F', ibid)
!
    call jedetr('&&XCONTA.NBSP')
    call detrsd('CHAM_ELEM_S', faclon)
    call detrsd('CHAM_ELEM_S', ainter)
!
    call jedema()
end subroutine

subroutine scalep(spectr, noma, base, nuor, nbm,&
                  imodi, nbmr, nbexcp, ltable, iaxe,&
                  scal)
    implicit none
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
!     PROJECTION D'UN SPECTRE D'EXCITATION TURBULENTE LOCALISEE SUR UNE
!     BASE MODALE PERTURBEE PAR COUPLAGE FLUIDE-STRUCTURE
!     CALCUL DES PRODUITS SCALAIRES PHII(XK).NK ET PHII'(XM).XM
!     APPELANT : SPECEP
!-----------------------------------------------------------------------
! IN  : SPECTR : NOM DU CONCEPT SPECTRE
! IN  : NOMA   : NOM DU CONCEPT MAILLAGE
! IN  : BASE   : NOM DU CONCEPT MELASFLU
! IN  : NUOR   : NUMEROS D'ORDRE DES MODES DU CONCEPT MELASFLU
! IN  : NBM    : NOMBRE DE MODES DU CONCEPT MELASFLU
! IN  : IMODI  : INDICE DU PREMIER MODE PRIS EN COMPTE
! IN  : NBMR   : NOMBRE DE MODES PRIS EN COMPTE
! IN  : NBEXCP : NOMBRE D'EXCITATIONS PONCTUELLES
! IN  : LTABLE : BOOLEEN, DONNE LE CAS DE CALCUL
!       LTABLE = .TRUE.  => SPECTRE DEFINI PAR L'UTILISATEUR
!       LTABLE = .FALSE. => RECOURS A UN SPECTRE GRAPPE2 PREDEFINI
! IN  : IAXE   : ENTIER DONNANT L'AXE DIRECTEUR DE LA POUTRE
! OUT : SCAL   : TABLEAU DES PRODUITS SCALAIRES (NBEXCP,NBMR)
!
!
#include "jeveux.h"
!
#include "asterc/r8dgrd.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelibe.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/wkvect.h"
    integer :: nbm, nuor(nbm), imodi, nbmr, nbexcp, iaxe
    logical(kind=1) :: ltable
    character(len=8) :: noma
    character(len=19) :: spectr, base
    real(kind=8) :: scal(nbexcp, nbmr)
!
    real(kind=8) :: dgrd
    character(len=24) :: spnnoe, spvare, spvate, nnoema, chvale
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: idec, idefm, idir1, idir2, iex, imod, imodf
    integer :: imr, inat, inatu, inuno, irot1, irot2, ispno
    integer :: ispre, ispte, iteta, iv, ivale, nuno
    real(kind=8) :: theta
!-----------------------------------------------------------------------
    call jemarq()
!
! --- 1.INITIALISATIONS
!
    dgrd = r8dgrd()
    imodf = imodi + nbmr - 1
    iv = 1
    if (iaxe .eq. 1) then
        idir1 = 2
        idir2 = 3
        irot1 = 6
        irot2 = 5
    else if (iaxe.eq.2) then
        idir1 = 3
        idir2 = 1
        irot1 = 4
        irot2 = 6
    else
        idir1 = 1
        idir2 = 2
        irot1 = 5
        irot2 = 4
    endif
!
! --- 2.RECUPERATIONS SIMULTANEES
!       - DES NUMEROS DES NOEUDS D'APPLICATION DES EXCITATIONS
!       - DES DIRECTIONS D'APPLICATION DE L'EXCITATION EN CHAQUE NOEUD
!       - DE LA NATURE DE L'EXCITATION EN CHAQUE NOEUD
!
    nnoema = noma//'.NOMNOE'
    spnnoe = spectr//'.NNOE'
    call jeveuo(spnnoe, 'L', ispno)
!
    call wkvect('&&SCALEP.TEMP.NUNO', 'V V I', nbexcp, inuno)
    call wkvect('&&SCALEP.TEMP.TETA', 'V V R', 2*nbexcp, iteta)
    call wkvect('&&SCALEP.TEMP.NATU', 'V V K8', nbexcp, inatu)
!
    if (ltable) then
!
        spvare = spectr//'.VARE'
        spvate = spectr//'.VATE'
        call jeveuo(spvare, 'L', ispre)
        call jeveuo(spvate, 'L', ispte)
!
        do 10 iex = 1, nbexcp
            call jenonu(jexnom(nnoema, zk8(ispno+iex-1)), zi(inuno+iex- 1))
            theta = zr(ispre+iex-1) * dgrd
            zr(iteta+2*(iex-1)) = dble(cos(theta))
            zr(iteta+2*(iex-1)+1) = dble(sin(theta))
            zk8(inatu+iex-1) = zk16(ispte+4+iex-1)(1:8)
10      continue
!
    else
!
        call jenonu(jexnom(nnoema, zk8(ispno)), nuno)
        do 11 iex = 1, nbexcp
            zi(inuno+iex-1) = nuno
            zr(iteta+2*(iex-1)) = 1.d0
            zr(iteta+2*(iex-1)+1) = 1.d0
            inat = iex - int(iex/2) * 2
            if (inat .eq. 0) then
                zk8(inatu+iex-1) = 'MOMENT'
            else
                zk8(inatu+iex-1) = 'FORCE'
            endif
11      continue
!
    endif
!
! --- 3.RECUPERATION DES DEFORMEES MODALES OU/ET DES DERIVEES DES
! ---   DEFORMEES MODALES AUX NOEUDS D'APPLICATION
!
    call wkvect('&&SCALEP.TEMP.DEFM', 'V V R', 2*nbexcp*nbmr, idefm)
!
    do 20 imod = imodi, imodf
!
        write(chvale,'(A8,A5,2I3.3,A5)') base(1:8),'.C01.',nuor(imod),&
        iv,'.VALE'
        call jeveuo(chvale, 'L', ivale)
        imr = imod - imodi + 1
!
        do 21 iex = 1, nbexcp
            idec = 2*nbexcp*(imr-1)+2*(iex-1)
            if (zk8(inatu+iex-1)(1:1) .eq. 'F') then
                zr(idefm+idec) = zr(ivale+6*(zi(inuno+iex-1)-1)+idir1- 1)
                zr(idefm+idec+1) = zr(ivale+6*(zi(inuno+iex-1)-1)+ idir2-1)
            else
                zr(idefm+idec) = zr(ivale+6*(zi(inuno+iex-1)-1)+irot1- 1)
                zr(idefm+idec+1) = zr(ivale+6*(zi(inuno+iex-1)-1)+ irot2-1)
            endif
21      continue
!
        call jelibe(chvale)
!
20  end do
!
! --- 4.CALCUL DES PRODUITS SCALAIRES
!
    do 30 imr = 1, nbmr
        do 31 iex = 1, nbexcp
            idec = 2*nbexcp*(imr-1)+2*(iex-1)
            scal(iex,imr) = zr(idefm+idec)*zr(iteta+2*(iex-1)) + zr(idefm+idec+1)*zr(iteta+2*(iex&
                            &-1)+1)
31      continue
30  end do
!
    call jedetr('&&SCALEP.TEMP.NUNO')
    call jedetr('&&SCALEP.TEMP.TETA')
    call jedetr('&&SCALEP.TEMP.NATU')
    call jedetr('&&SCALEP.TEMP.DEFM')
    call jedema()
end subroutine

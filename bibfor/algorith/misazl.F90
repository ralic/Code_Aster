subroutine misazl(vecinc, defico)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    implicit none
#include "jeveux.h"
!
#include "asterc/indik8.h"
#include "asterfort/assert.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfnumn.h"
#include "asterfort/cftypn.h"
#include "asterfort/dismoi.h"
#include "asterfort/iposdg.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
    character(len=19) :: vecinc
    character(len=24) :: defico
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - UTILITAIRE)
!
! MISE A ZERO DES LAGRANGIENS CONTACT/FROTTEMENT DANS VECTEUR INCONNUES
!
! ----------------------------------------------------------------------
!
! IN  DEFICO : SD DE DEFINITION DU CONTACT
! I/O VECINC : VECTEUR DES INCONNUES
!
!
!
!
    integer :: nnoco, ntnoe
    integer :: ino, nbno, nec, ncmpmx, numno(1)
    integer :: ibid, ier, ino_ind(1)
    integer :: jnocmp, jprno, jnueq, jvale, jdg
    integer :: numlc, numlf1, numlf2
    integer :: poslc, poslf1, poslf2
    integer :: inueq, ivalc, ivalf1, ivalf2
    character(len=4) :: typno
    character(len=8) :: nomgd, kbid
    character(len=19) :: prno
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    nnoco = cfdisi(defico,'NNOCO')
    ntnoe = cfdisi(defico,'NTNOE')
    nbno = 0
!
! --- ACCES AU NOM DE LA GRANDEUR
!
    call dismoi('F', 'NOM_GD', vecinc, 'CHAM_NO', ibid,&
                nomgd, ier)
!
! --- ACCES AU NOM ET AU NOMBRE DES COMPOSANTES DE LA GRANDEUR
!
    call jeveuo(jexnom('&CATA.GD.NOMCMP', nomgd), 'L', jnocmp)
    call dismoi('F', 'NB_CMP_MAX', nomgd, 'GRANDEUR', ncmpmx,&
                kbid, ier)
!
! --- ACCES AU PROF_CHNO ET AU POINTEUR .NUEQ
!
    call dismoi('F', 'PROF_CHNO', vecinc, 'CHAM_NO', ibid,&
                prno, ier)
    call jeveuo(jexnum(prno//'.PRNO', 1), 'L', jprno)
    call jeveuo(prno//'.NUEQ', 'L', jnueq)
!
! --- ACCES AU NOMBRE D'ENTIERS CODES POUR PARCOURIR LE .PRNO
!
    call dismoi('F', 'NB_EC', nomgd, 'GRANDEUR', nec,&
                kbid, ier)
!
! --- ACCES AU .VALE DU CHAMP VECINC
!
    call jeveuo(vecinc//'.VALE', 'E', jvale)
!
! -- RECHERCHE DES NUMEROS DES COMPOSANTES DANS LA GRANDEUR
!
    numlc = indik8(zk8(jnocmp),'LAGS_C' ,1,ncmpmx)
    numlf1 = indik8(zk8(jnocmp),'LAGS_F1',1,ncmpmx)
    numlf2 = indik8(zk8(jnocmp),'LAGS_F2',1,ncmpmx)
!
! --- PARCOURS DES NOEUDS ESCLAVES ET ANNULATION DES LAGR. DANS LE .VALE
!
    do ino = 1, nnoco
!       NOEUDS ESCLAVES
        call cftypn(defico, ino, typno)
        if (typno .eq. 'ESCL') then
            nbno = nbno+1
!         NUMERO ABSOLU DU NOEUD ESCLAVE
            ino_ind(1) = ino
            call cfnumn(defico, 1, ino_ind, numno)
!         DEBUT DU DESCRIPTEUR GRANDEUR DU NOEUD ESCLAVE
            jdg = jprno - 1 + (numno(1)-1)*(2+nec) + 1 + 2
!         POSITIONS DES LAGRANGES DANS LE DG
            poslc = iposdg(zi(jdg),numlc )
            poslf1 = iposdg(zi(jdg),numlf1)
            poslf2 = iposdg(zi(jdg),numlf2)
!         INDIRECTION VERS LE .NUEQ
            inueq = zi(jprno - 1 + (numno(1)-1)*(2+nec) + 1)
            ASSERT(poslc.ne.0)
!         ADRESSE DU DDL LAGS_C DANS LE .VALE
            ivalc = zi(jnueq - 1 + inueq - 1 + poslc)
            zr(jvale - 1 + ivalc) = 0.d0
            if (poslf1 .ne. 0) then
!           ADRESSE DU DDL LAGS_F1 DANS LE .VALE
                ivalf1 = zi(jnueq - 1 + inueq - 1 + poslf1)
                zr(jvale - 1 + ivalf1) = 0.d0
                if (poslf2 .ne. 0) then
!              ADRESSE DU DDL LAGS_F2 DANS LE .VALE
                    ivalf2 = zi(jnueq - 1 + inueq - 1 + poslf2)
                    zr(jvale - 1 + ivalf2) = 0.d0
                endif
            endif
        endif
    end do
    ASSERT(nbno.eq.ntnoe)
!
    call jedema()
end subroutine

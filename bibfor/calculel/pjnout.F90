subroutine pjnout(modele)
    implicit none
    include 'jeveux.h'
!
    include 'asterfort/dismoi.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexatr.h'
    include 'asterfort/wkvect.h'
    character(len=8) :: modele
!     ------------------------------------------------------------------
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
!     ------------------------------------------------------------------
!
!     BUT : CREER SUR LA BASE 'V' L'OBJET SD_MODELE.NOEUD_UTIL
!
!     LONG(.NOEUD_UTIL)= NB_NO(MAILLAGE(MODELE))
!     NOEUD_UTIL(INO) = 1 : INO APPARTIENT A UN ELEMENT
!                           DU LIGREL DU MODELE
!     NOEUD_UTIL(INO) = 0 =>: SINON
!     ------------------------------------------------------------------
!
    character(len=8) :: k8b, noma
    integer :: nbnoeu, ibid, jnout, ima, nbno, j, jmaill, imail, nbmail
!     ------------------------------------------------------------------
!
!     FONCTIONS "FORMULES" POUR ACCEDER RAPIDEMENT A LA CONNECTIVITE :
    integer :: iconx1, iconx2, zzconx, zznbne
    zzconx(imail,j) = zi(iconx1-1+zi(iconx2+imail-1)+j-1)
    zznbne(imail) = zi(iconx2+imail) - zi(iconx2+imail-1)
!     ------------------------------------------------------------------
!
!
    call jemarq()
!
    call dismoi('F', 'NOM_MAILLA', modele, 'MODELE', ibid,&
                noma, ibid)
    call dismoi('F', 'NB_NO_MAILLA', modele, 'MODELE', nbnoeu,&
                k8b, ibid)
    call jedetr(modele//'.NOEUD_UTIL')
    call wkvect(modele//'.NOEUD_UTIL', 'V V I', nbnoeu, jnout)
!
    call dismoi('F', 'NB_MA_MAILLA', modele, 'MODELE', nbmail,&
                k8b, ibid)
    if (nbmail .eq. 0) goto 290
!
    call jeveuo(noma//'.CONNEX', 'L', iconx1)
    call jeveuo(jexatr(noma//'.CONNEX', 'LONCUM'), 'L', iconx2)
    call jeveuo(modele//'.MAILLE', 'L', jmaill)
!
    do 280 ima = 1, nbmail
        if (zi(jmaill+ima-1) .eq. 0) goto 280
        nbno = zznbne(ima)
        do 270 j = 1, nbno
            zi(jnout-1+zzconx(ima,j)) = 1
270      continue
280  end do
290  continue
!
    call jedema()
end subroutine

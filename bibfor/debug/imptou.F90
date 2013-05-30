subroutine imptou(base, tous, mess)
    implicit none
    include 'jeveux.h'
!
    include 'asterfort/dbgobj.h'
    include 'asterfort/jecreo.h'
    include 'asterfort/jecroc.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeecra.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelstc.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenonu.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/wkvect.h'
    character(len=*) :: base, tous, mess
! ----------------------------------------------------------------------
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
! BUT : IMPRIMER LA SIGNATURE DE TOUS LES OBJETS PRESENTS SUR UNE BASE.
!
! BASE   IN  K1 : NOM DE LA BASE : 'G'/'V'/'L'/' '
! TOUS   IN  K3 :  / 'ALL' : ON IMPRIME TOUS LES OBJEST TROUVES
!                  / 'NEW' : ON N'IMPRIME QU ELES OBJETS "NOUVEAUX"
!                            (DEPUIS LE DERNIER APPEL A IMPTOU)
! MESS   IN  K* :  MESSAGE PREFIXANT CHAQUE LIGNE
! ----------------------------------------------------------------------
    character(len=1) :: bas1
    character(len=8) :: kbid
    character(len=24) :: obj, dejavu
    integer :: i, iret, nbval, nbobj, ialiob
    integer :: nuobj
!
    call jemarq()
!     GOTO 9999
    bas1=base
!
!
!     1. LA PREMIERE FOIS ON ALLOUE UN POINTEUR DE NOMS QUI CONTIENDRA
!        TOUS LES OBJETS DEJA IMPRIMES UNE FOIS : '&&IMPTOU.DEJAVU'
!     --------------------------------------------------------------
    dejavu='&&IMPTOU.DEJAVU'
    call jeexin(dejavu, iret)
    if (iret .eq. 0) then
        call jecreo(dejavu, 'G N K24')
        call jeecra(dejavu, 'NOMMAX', 90000, kbid)
    endif
!
!
!
!     2. RECUPERATION DE LA LISTE DES OBJETS :
!     --------------------------------------------------------------
    call jelstc(bas1, ' ', 0, 0, kbid,&
                nbval)
    nbobj = -nbval
!     -- ON AUGMENTE NBOBJ DE 1 CAR ON VA CREER UN OBJET DE PLUS !
    nbobj = nbobj+1
    call wkvect('&&IMPTOU.LISTE', 'V V K24', nbobj, ialiob)
    call jelstc(bas1, ' ', 0, nbobj, zk24(ialiob),&
                nbval)
!
!
!     3. IMPRESSION DE LA SIGNATURE DES OBJETS :
!     --------------------------------------------------------------
    do 10 i = 1, nbobj
        obj = zk24(ialiob-1+i)
        call jenonu(jexnom(dejavu, obj), nuobj)
        if (nuobj .eq. 0) call jecroc(jexnom(dejavu, obj))
!
        if ((nuobj.gt.0) .and. (tous.eq.'NEW')) goto 10
!
        call dbgobj(obj, 'OUI', 6, mess)
10  end do
!
!
!     4. MENAGE
!     --------------------------------------------------------------
    call jedetr('&&IMPTOU.LISTE')
!
    call jedema()
!
end subroutine

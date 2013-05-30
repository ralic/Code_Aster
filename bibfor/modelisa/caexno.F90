subroutine caexno(lvavz, nomaz, motfac, mcgrno, mcno,&
                  iocc)
    implicit none
    include 'jeveux.h'
!
    include 'asterc/getfac.h'
    include 'asterfort/assert.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/palino.h'
    character(len=24) :: lvav, noma
    character(len=*) :: motfac, mcgrno, mcno, lvavz, nomaz
    integer :: iocc
! ---------------------------------------------------------------------
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
!                 DATE 04/01/93   C.MENGONI
!
!     BUT : RETIRER DE LA LISTE DE VIS A VIS LVAV TOUS LES COUPLES
!           DONT UN DES TERMES EST APPARTIENT A LA LISTE DE NOEUDS
!           GENEREE PAR LES MOTS CLES MCGRNO ET MCNO POUR L'OCCURENCE
!           IOCC DU MOT CLE FACTEUR MOTFAC.
! IN  LVAVZ  K*(*): NOM UTILISATEUR DE LA LISTE DES VIS A VIS
!            OJB V V I DIM = 2 * NBCOUPLE + 1
!            LVAV(ILAD) = NBCOUPLE (BOUCLE SUR I <= NBCOUPLE)
!            LVAV(ILAD+2*(I-1)+1) = NUM1 DU NOEUD 1
!            LVAV(ILAD+2*(I-1)+2) = NUM2 DU NOEUD 2
!                   OU PREFIXE DES OJB .CONI ET .CONR
! IN  NOMAZ  K*(*): NOM DU MAILLAGE
! IN  MOTFAC K16  : MOT CLE FACTEUR A TRAITER
! IN  MCGRNO K16  : MOT CLE REGROUPANT LES GROUP_NO
! IN  MCNO   K16  : MOT CLE REGROUPANT LES NOEUDS
! IN  IOCC   I    : SI >0 ON TRAITE L'OCCURENCE IOCC DE MOTFAC
!                   SI <0 OU =0 ERREUR FATALE
!
!
!
    character(len=16) :: motf, mcgr, mcn
    character(len=24) :: listex
    integer :: nbcpl, nbex
! --- DEBUT
!-----------------------------------------------------------------------
    integer :: i, idlex, idlvav, j, l, nlino
!-----------------------------------------------------------------------
    call jemarq()
    lvav = lvavz
    noma = nomaz
    motf = motfac
    mcgr = mcgrno
    mcn = mcno
    if (motf .ne. 'LIAISON_GROUP') then
        call assert(.false.)
    endif
    if (iocc .le. 0) then
        call assert(.false.)
    endif
    call getfac(motf, nlino)
    if ((nlino.eq.0) .or. (iocc.gt.nlino)) goto 999
!
! --- LECTURE DE LA LISTE DES NOEUDS EXCLUS
!
    listex = '&&CAEXNO.LISTENOEUD'
    call palino(noma, motfac, mcgr, mcn, iocc,&
                listex)
!
! --- ELIMINATION
!
    call jeveuo(listex, 'L', idlex)
    nbex = zi(idlex)
    if (nbex .eq. 0) goto 998
    call jeveuo(jexnum(lvav, iocc), 'E', idlvav)
    nbcpl = zi(idlvav)
    l = 0
    do 2 i = 1, nbcpl
        l = l+1
        do 3 j = 1, nbex
            if ((zi(idlvav+2*(i-1)+1).eq.zi(idlex+j)) .or.&
                (zi(idlvav+ 2*(i-1)+2).eq.zi(idlex+j))) then
                l = l-1
                goto 2
            endif
 3      continue
        zi(idlvav+2*(l-1)+1) = zi(idlvav+2*(i-1)+1)
        zi(idlvav+2*(l-1)+2) = zi(idlvav+2*(i-1)+2)
 2  end do
    zi(idlvav) = l
998  continue
    call jedetr('&&CAEXNO.LISTENOEUD')
999  continue
! FIN -----------------------------------------------------------------
    call jedema()
end subroutine

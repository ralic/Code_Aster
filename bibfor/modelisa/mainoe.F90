subroutine mainoe(noma, nbm, limanu, nuouno, nbno,&
                  obj)
    implicit   none
    include 'jeveux.h'
!
    include 'asterfort/assert.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexatr.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/wkvect.h'
    integer :: limanu(*), nbm
    character(len=24) :: obj
    character(len=8) :: noma
    character(len=2) :: nuouno
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!
! BUT : FAIRE LA LISTE DES NOEUDS PORTES PAR LES MAILLES DE LIMANU
!       (SANS DOUBLONS)
!
! ARGUMENTS D'ENTREE:
!      NOMA : NOM DU MAILLAGE
!      NBM  : NOMBRE DE MAILLES DANS LA LISTE.
!    LIMANU : LISTE DES NUMEROS DE MAILLE
!    NUOUNO : /'NU' : ON VEUT LA LISTE DES NUMEROS DE NOEUDS
!             /'NO' : ON VEUT LA LISTE DES NOMS DE NOEUDS
!     OBJ   : NOM DE L'OBJET QUI CONTIENDRA LES NOEUDS (V I OU V K8)
!
! ARGUMENTS DE SORTIE:
!      NBNO : NOMBRE DE NOEUDS TROUVES
!      OBJ  : OBJ EST ALLOUE ET REMPLI
!-----------------------------------------------------------------------
    integer :: ier, nbnot, jconx1, jconx2, k, jlisno, ima, nbno, kno, nuno
    integer :: jresu, ico
    character(len=8) :: nono
    character(len=1) :: kbid
!     ------------------------------------------------------------------
!
    call jemarq()
    call assert(nuouno.eq.'NO' .or. nuouno.eq.'NU')
!
    call dismoi('F', 'NB_NO_MAILLA', noma, 'MAILLAGE', nbnot,&
                kbid, ier)
    call assert(nbnot.gt.0)
    call jeveuo(noma//'.CONNEX', 'L', jconx1)
    call jeveuo(jexatr(noma//'.CONNEX', 'LONCUM'), 'L', jconx2)
!
!
!     -- ON "COCHE" DANS .LISNO LES NOEUDS CONCERNES PAR LES MAILLES:
!     ----------------------------------------------------------------
    call jedetr('&&MAINOE.LISNO')
    call wkvect('&&MAINOE.LISNO', 'V V I', nbnot, jlisno)
    do 20,k = 1,nbm
    ima = limanu(k)
    nbno = zi(jconx2-1+ima+1) - zi(jconx2-1+ima)
    do 10,kno = 1,nbno
    nuno = zi(jconx1-1+zi(jconx2-1+ima)+kno-1)
    zi(jlisno-1+nuno) = 1
10  continue
    20 end do
!
!     -- ON COMPTE LES RETENUS  :
    nbno = 0
    do 30,k = 1,nbnot
    if (zi(jlisno-1+k) .eq. 1) nbno = nbno + 1
    30 end do
    call assert(nbno.gt.0)
!
!     -- ALLOCATION ET REMPLISSAGE DE OBJ :
!     --------------------------------------
    if (nuouno .eq. 'NU') then
        call wkvect(obj, 'V V I', nbno, jresu)
!
    else
        call wkvect(obj, 'V V K8', nbno, jresu)
    endif
!
    ico = 0
    do 40,k = 1,nbnot
    if (zi(jlisno-1+k) .eq. 0) goto 40
    ico = ico + 1
    if (nuouno .eq. 'NU') then
        zi(jresu-1+ico) = k
    else
        call jenuno(jexnum(noma//'.NOMNOE', k), nono)
        zk8(jresu-1+ico) = nono
    endif
    40 end do
!
    call jedema()
end subroutine

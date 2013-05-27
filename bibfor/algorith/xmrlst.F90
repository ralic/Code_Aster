subroutine xmrlst(jcesd, jcesv, jcesl, noma, posma,&
                  coor, lst)
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
! person_in_charge: patrick.massin at edf.fr
!
!
    implicit none
!
    include 'jeveux.h'
!
    include 'asterfort/assert.h'
    include 'asterfort/cesexi.h'
    include 'asterfort/elrfvf.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexatr.h'
    include 'asterfort/jexnum.h'
    character(len=8) :: noma
    integer :: jcesd(10), jcesv(10), jcesl(10)
    integer :: posma
    real(kind=8) :: coor(3), lst
!
!-----------------------------------------------------------------------
!
! ROUTINE XFEM (CONTACT - GRANDS GLISSEMENTS)
!
! CALCUL DE LA LST AU POINT D'INTEGRATION D'UN ELEMENT DONNEE
! SERT ENSUITE DANS LES TE POUR LES POINTES DE FISSURES
!
! ----------------------------------------------------------------------
! ROUTINE SPECIFIQUE A L'APPROCHE <<GRANDS GLISSEMENTS AVEC XFEM>>,
! TRAVAIL EFFECTUE EN COLLABORATION AVEC I.F.P.
! ----------------------------------------------------------------------
!
!  JCES*(7)  : POINTEURS DE LA SD SIMPLE DE LA LEVEL SET TANGENTE
! IN  NOMA   : NOM DU MAILLAGE
! IN  POSMA  : POSITION DE LA MAILLE ESCLAVE OU MAITRE
! IN  COOR   : COORDONNEES DU POINT DANS L'ELEMENT PARENT
! OUT  LST   : LEVEL SET TANGENTE AU POINT D'INTEGRATION
!
!
!
!
    real(kind=8) :: ff(20)
    integer :: jconx1, jconx2, jma
    integer :: itypma, nno, ino, iad
    character(len=8) :: typma, elref, k8bid
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- ON RECUPERE LA CONNECTIVITE DU MAILLAGE
!
    call jeveuo(noma//'.CONNEX', 'L', jconx1)
    call jeveuo(jexatr(noma//'.CONNEX', 'LONCUM'), 'L', jconx2)
!
    call jeveuo(noma//'.TYPMAIL', 'L', jma)
!
! --- ON RECUPERE LE TYPE DE LA MAILLE
!
    call jeveuo(noma//'.TYPMAIL', 'L', jma)
    itypma=zi(jma-1+posma)
    call jenuno(jexnum('&CATA.TM.NOMTM', itypma), typma)
    if (typma .eq. 'HEXA8') elref = 'HE8'
    if (typma .eq. 'PENTA6') elref = 'PE6'
    if (typma .eq. 'TETRA4') elref = 'TE4'
    if (typma .eq. 'HEXA20') elref = 'H20'
    if (typma .eq. 'PENTA15') elref = 'P15'
    if (typma .eq. 'TETRA10') elref = 'T10'
    if (typma(1:4) .eq. 'QUAD') elref = 'QU4'
    if (typma(1:4) .eq. 'TRIA') elref = 'TR3'
!
! --- ON RECUPERE LE NOMBRE DE NOEUDS DE LA MAILLE
!
    call jelira(jexnum(noma//'.CONNEX', posma), 'LONMAX', nno, k8bid)
!
! --- FONCTIONS DE FORMES DU PT DE CONTACT DANS L'ELE PARENT
!
    call elrfvf(elref, coor, nno, ff, nno)
!
! --- ON INTERPOLE LA LST AVEC SES VALEURS AUX NOEUDS
!
!
    lst = 0.d0
    do 10 ino = 1, nno
        call cesexi('C', jcesd(7), jcesl(7), posma, ino,&
                    1, 1, iad)
        call assert(iad.gt.0)
        lst = lst + zr(jcesv(7)-1+iad) * ff(ino)
10  end do
    lst = sqrt(abs(lst))
!
    call jedema()
end subroutine

subroutine nmextj(nomcha, nbcmp, listcp, extrcp, num,&
                  snum, nvalcp, nummai, jcesd, jcesv,&
                  jcesl, jcesc, valres)
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit      none
    include 'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/cesexi.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/lxliis.h'
    include 'asterfort/nmextv.h'
    character(len=24) :: nomcha
    integer :: nbcmp
    integer :: nvalcp
    real(kind=8) :: valres(*)
    character(len=24) :: listcp
    character(len=8) :: extrcp
    integer :: num, snum
    integer :: nummai
    integer :: jcesd, jcesv, jcesl, jcesc
!
! ----------------------------------------------------------------------
!
! ROUTINE *_NON_LINE (EXTRACTION - UTILITAIRE)
!
! EXTRAIRE LES VALEURS DES COMPOSANTES - ELEMENT
!
! ----------------------------------------------------------------------
!
!
! IN  EXTRCP : TYPE D'EXTRACTION SUR LES COMPOSANTES
! IN  NOMCHA : NOM DU CHAMP
! IN  NBCMP  : NOMBRE DE COMPOSANTES
! IN  LISTCP : LISTE DES COMPOSANTES
! IN  NUM    : NUMERO POINT DE GAUSS
! IN  SNUM   : NUMERO SOUS-POINT DE GAUSS
! IN  JCESD  : ADRESSE ACCES CHAM_ELEM_S.CESD
! IN  JCESV  : ADRESSE ACCES CHAM_ELEM_S.CESV
! IN  JCESL  : ADRESSE ACCES CHAM_ELEM_S.CESL
! IN  JCESC  : ADRESSE ACCES CHAM_ELEM_S.CESC
! OUT VALRES : VALEUR DES COMPOSANTES
! OUT NVALCP : NOMBRE EFFECTIF DE COMPOSANTES
!
!
!
!
    integer :: nparx
    parameter    (nparx=20)
    character(len=8) :: nomcmp(nparx)
    real(kind=8) :: valcmp(nparx)
    integer :: neff, nbcmpx
    integer :: icmp, ipar, iret, ieff, i
    integer :: jcmp, iad
    character(len=8) :: cmp, nomvar
    integer :: ivari
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    ieff = 1
    nbcmpx = zi(jcesd+4)
    call assert(nbcmp.le.nparx)
!
! --- NOM DES COMPOSANTES
!
    call jeveuo(listcp, 'L', jcmp)
!
    do 20 icmp = 1, nbcmp
        nomcmp(icmp) = zk8(jcmp-1+icmp)
20  end do
!
! --- VALEURS DES COMPOSANTES
!
    do 30 ipar = 1, nbcmp
        cmp = nomcmp(ipar)
        if (nomcha(1:4) .eq. 'VARI') then
            nomvar = cmp(2:8)//' '
            call lxliis(nomvar, ivari, iret)
        else
            ivari = 0
        endif
        if (nomcha(1:4) .eq. 'VARI') then
            icmp = ivari
        else
            do 40 i = 1, nbcmpx
                if (cmp .eq. zk8(jcesc-1+i)) icmp=i
40          continue
        endif
        call cesexi('C', jcesd, jcesl, nummai, num,&
                    snum, icmp, iad)
        if (iad .gt. 0) then
            valcmp(ieff) = zr(jcesv+iad-1)
            ieff = ieff + 1
        endif
30  end do
    neff = ieff - 1
!
! --- EVALUATION
!
    call nmextv(neff, extrcp, nomcmp, valcmp, nvalcp,&
                valres)
!
    call assert(nvalcp.le.nbcmp)
!
    call jedema()
!
end subroutine

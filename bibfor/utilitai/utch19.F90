subroutine utch19(cham19, nomma, nomail, nonoeu, nupo,&
                  nusp, ivari, nocmp, typres, valr,&
                  valc, vali, ier)
    implicit   none
    include 'jeveux.h'
    include 'asterc/r8vide.h'
    include 'asterfort/assert.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/mpicm1.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/utchdl.h'
    integer :: nupo, ivari, ier, nusp, vali
    real(kind=8) :: valr
    complex(kind=8) :: valc
    character(len=*) :: cham19, nomma, nomail, nonoeu, nocmp, typres
! ----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     EXTRAIRE UNE VALEUR DANS UN CHAM_ELEM.
! ----------------------------------------------------------------------
! IN  : CHAM19 : NOM DU CHAM_ELEM DONT ON DESIRE EXTRAIRE 1 COMPOSANTE
! IN  : NOMMA  : NOM DU MAILLAGE
! IN  : NOMAIL : NOM DE LA MAILLE A EXTRAIRE
! IN  : NONOEU : NOM D'UN NOEUD (POUR LES CHAM_ELEM "AUX NOEUDS").
!                  (SI CE NOM EST BLANC : ON UTILISE NUPO)
! IN  : NUPO   : NUMERO DU POINT A EXTRAIRE SUR LA MAILLE NOMAIL
! IN  : NUSP   : NUMERO DU SOUS_POINT A TESTER SUR LA MAILLE NOMAIL
!                (SI NUSP=0 : IL N'Y A PAS DE SOUS-POINT)
! IN  : IVARI  : NUMERO DE LA CMP (POUR VARI_R)
! IN  : NOCMP : NOM DE LA CMP A EXTRAIRE SUR LE POINT NUPO
! IN  : TYPRES : TYPE DU CHAMP ET DU RESULTAT (R/C).
! OUT : VALR   : VALEUR EXTRAITE (SI REEL)
! OUT : VALC   : VALEUR EXTRAITE (SI COMPLEXE)
! OUT : VALI   : VALEUR EXTRAITE (SI ENTIER)
! OUT : IER    : CODE RETOUR.
! ----------------------------------------------------------------------
!
    integer :: ibid, icmp, jcelv
    real(kind=8) :: r1, r2
    complex(kind=8) :: cbid
    character(len=1) :: typrez
    character(len=4) :: type, kmpic
    character(len=19) :: chm19z
!     ------------------------------------------------------------------
!
    call jemarq()
    ier = 0
!
    chm19z = cham19(1:19)
    typrez = typres(1:1)
    call jelira(chm19z//'.CELV', 'TYPE', ibid, type)
!
    call assert(type.eq.typrez)
    call dismoi('F', 'MPI_COMPLET', cham19, 'CHAM_ELEM', ibid,&
                kmpic, ibid)
    call assert(kmpic.eq.'OUI'.or.kmpic.eq.'NON')
!
    if (type .ne. 'R' .and. type .ne. 'C' .and. type .ne. 'I') call u2mesk('E', 'UTILITAI5_29',&
                                                                           1, type)
!
    call utchdl(cham19, nomma, nomail, nonoeu, nupo,&
                nusp, ivari, nocmp, icmp)
!
!     SI TEST_RESU, ICMP PEUT ETRE = 0 :
    if (icmp .eq. 0) then
        ier=1
        valr=r8vide()
        valc=dcmplx(r8vide(),r8vide())
        goto 10
    endif
!
    call jeveuo(chm19z//'.CELV', 'L', jcelv)
    if (typrez .eq. 'R') then
        valr = zr(jcelv-1+icmp)
    else if (typrez.eq.'C') then
        valc = zc(jcelv-1+icmp)
    else if (typrez.eq.'I') then
        vali = zi(jcelv-1+icmp)
    endif
!
!     -- SI LE CHAMP N'EST PAS MPI_COMPLET, IL FAUT COMMUNIQUER
!        LA VALEUR EXTRAITE :
    if (kmpic .eq. 'NON') then
        if (typrez .eq. 'R') then
            call mpicm1('MPI_SUM', 'R', 1, ibid, ibid,&
                        valr, cbid)
        else if (typrez.eq.'C') then
            r1=dble(valc)
            r2=dimag(valc)
            call mpicm1('MPI_SUM', 'R', 1, ibid, ibid,&
                        r1, cbid)
            call mpicm1('MPI_SUM', 'R', 1, ibid, ibid,&
                        r2, cbid)
            valc=dcmplx(r1,r2)
        endif
    endif
!
10  continue
    call jedema()
end subroutine

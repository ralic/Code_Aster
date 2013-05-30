subroutine mltblc(nbsn, debfsn, mxbloc, seq, nbloc,&
                  decal, lgbloc, ncbloc)
! person_in_charge: olivier.boiteau at edf.fr
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
    implicit none
!       CALCUL DU NOMBRE DE BLOCS POUR LA MATRICE FACTORISEE
!       DONNEES :
!                 MXBLOC : LONGUEUR MAXIMUM D'UN BLOC
!       RESULTATS :
!                   NBLOC NBRE DE BLOCS
!                   NOBLOC(NBLOC) : NUMERO DE BLOC DE CHQUE SND
!                   LGBLOC(NBLOC) : NBRE DE COEFFICIENTS DE CHAQUE BLOC
!                   DECAL(NBSN) :  DEBUT DE CHAQUE SNOEUD DANS LE TABLEA
!                                    FACTOR QUI CONTIENT LES BLOCS
    include 'asterc/ismaem.h'
    include 'asterc/lor8em.h'
    include 'asterfort/u2mesg.h'
    include 'asterfort/u2mesi.h'
    integer :: nbsn, seq(nbsn), debfsn(nbsn+1), mxbloc, nbloc, decal(nbsn)
    integer :: lgbloc(*), ncbloc(*)
    integer :: i, l, i0, long
    integer :: vali(3), lm, lr
!-----------------------------------------------------------------------
    integer :: ib, ni
!-----------------------------------------------------------------------
    lm=ismaem()
    lr=lor8em()
    nbloc = 1
    i0 = 1
110  continue
    i = i0
    decal(seq(i)) = 1
    long = debfsn(seq(i)+1) - debfsn(seq(i))
    if (long .gt. mxbloc) then
        vali (1) = mxbloc
        vali (2) = i
        vali (3) = long
        call u2mesg('F', 'ALGELINE4_21', 0, ' ', 3,&
                    vali, 0, 0.d0)
    endif
!      DO WHILE (LONG.LE.MXBLOC)
120  continue
    if (long .le. mxbloc) then
        if (i .eq. nbsn) goto 130
        i = i + 1
        decal(seq(i)) = long + 1
        l = debfsn(seq(i)+1) - debfsn(seq(i))
        if (l .gt. mxbloc) then
            vali (1) = mxbloc
            vali (2) = i
            vali (3) = l
            call u2mesg('F', 'ALGELINE4_21', 0, ' ', 3,&
                        vali, 0, 0.d0)
        endif
        long = long + l
        goto 120
! FIN DO WHILE
    endif
!      CHAQUE BLOC VA DES NUMEROS DE SNDS SEQ(I0) A SEQ(I-1)
    ncbloc(nbloc) = i - i0
    lgbloc(nbloc) = long - l
    nbloc = nbloc + 1
    i0 = i
    goto 110
!
130  continue
    ncbloc(nbloc) = nbsn - i0 + 1
    lgbloc(nbloc) = long
!
    do 140 ib = 1, nbloc
        if (lgbloc(ib) .gt. lm/lr) then
            ni=3
            vali(1)=ib
            vali(2)=lgbloc(ib)
            vali(3)=lm
            call u2mesi('A', 'ALGELINE3_52', ni, vali)
!
        endif
140  end do
end subroutine

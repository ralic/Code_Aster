subroutine irgme2(numold, ima, connex, nbord2, tabd,&
                  tabl, tabv, partie, jtype, nbno,&
                  listno, nbcmp, ifi, iadmax)
    implicit none
    include 'jeveux.h'
!
    include 'asterfort/cesexi.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/u2mess.h'
    integer :: numold(*), tabd(*), tabl(*), tabv(*), nbno
    integer :: listno(*), nbcmp, ifi, ima, nbord2, iadmax, jtype
    character(len=24) :: connex
    character(len=*) :: partie
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
!
!     BUT: ECRITURE DES CMP D'UN CHAMP TENSORIEL PAR ELEMENT
!     POUR UN TYPE D'ELEMENT AU FORMAT GMSH
!
!     ENTREE:
!     NUMOLD : I   : TABLEAU DE CORRESPONDANCE NOUV MAILLE ANC. MAILLE
!     IMA    : I   : NUMERO NOUVELLE MAILLE
!     CONNEX : I   : CONNECTIVITE ANCIEN MAILLAGE
!     NBORD2 : I   : NOMBRE DE NUM D'ORDRE
!     TABD   : I   : DESCRIPTEURS DU CHAMP SIMPLE A IMPRIMER
!     TABL   : I   : DESCRIPTEURS DU CHAMP SIMPLE A IMPRIMER
!     TABV   : I   : DESCRIPTEURS DU CHAMP SIMPLE A IMPRIMER
!     PARTIE : K4  : IMPRESSION DE LA PARTIE COMPLEXE OU REELLE DU CHAMP
!     JTYPE  : I   : ADRESSE DU TYPE DU CHAMP ( REEL OU COMPLEXE )
!     NBNO   : I   : NOMBRE NOEUD DE LA NOUVELLE MAILLE
!     LISTNO : I   : LISTE DES NOEUDS DE LA NOUVELLE MAILLE
!     NBCMP  : I   : NOMBRE DE COMPOSANTES DU CHAMP
!     IFI    : I   : NUMERO D'UNITE LOGIQUE DU FICHIER GMSH
!     SORTIE
!     IADMAX  : I   : MAX DES IAD SI >0 LE CHAMP EXISTE POUR LA MAILLE
!
!     ------------------------------------------------------------------
    integer :: imaold, jcnold, ior, jcesd, jcesl, jcesv, nbpt, nbsp, j, ino
    integer :: itrou, ipt, inold, isp, iad, k
    real(kind=8) :: vale
    real(kind=8) :: val2(6)
!
!     ------------------------------------------------------------------
!
    call jemarq()
!
    imaold = numold(ima)
    call jeveuo(jexnum(connex, imaold), 'L', jcnold)
!
! --- ON NE TRAITE QUE LES CHAMPS A 1 SOUS-POINT,
!     ET UNE SEULE VALEUR SCALAIRE (COMPOSANTE K DE LA BOUCLE 51)
!
    isp=1
    iadmax=0
    k = 0
    do 11 ior = 1, nbord2
        jcesd = tabd(ior)
        jcesl = tabl(ior)
        jcesv = tabv(ior)
        nbpt = zi(jcesd-1+5+4*(imaold-1)+1)
        nbsp = zi(jcesd-1+5+4*(imaold-1)+2)
        if (nbsp .ne. 1) then
            call u2mess('F', 'PREPOST2_57')
        endif
!
        itrou=0
        if (zk8(jtype-1+ior) .eq. 'R') then
            do 14 j = 1, nbno
                ino=listno(j)
                itrou=0
                do 13 ipt = 1, nbpt
                    inold=zi(jcnold-1+ipt)
                    if (ino .eq. inold) then
                        itrou=1
                        do 16 k = 1, nbcmp
                            call cesexi('C', jcesd, jcesl, imaold, ipt,&
                                        isp, k, iad)
                            if (iad .gt. 0) then
                                vale = zr(jcesv-1+iad)
                                if (abs(vale) .le. 1.d-99) vale = 0.d0
                                val2(k) = vale
                                iadmax=iad
                            else
                                vale = 0.d0
                                val2(k) = vale
                            endif
16                      continue
                    endif
                    write(ifi,1010) val2(1),val2(4),val2(5),val2(4),&
                    val2(2), val2(6),val2(5),val2(6),val2(3)
                    goto 15
13              continue
                if (itrou .eq. 0) then
                    call u2mess('F', 'PREPOST2_58')
                endif
15              continue
14          continue
        else if (zk8(jtype-1+ior).eq.'C') then
            do 24 j = 1, nbno
                ino=listno(j)
                itrou=0
                do 23 ipt = 1, nbpt
                    inold=zi(jcnold-1+ipt)
                    if (ino .eq. inold) then
                        itrou=1
                        do 26 k = 1, nbcmp
                            call cesexi('C', jcesd, jcesl, imaold, ipt,&
                                        isp, k, iad)
                            if (iad .gt. 0) then
                                if (partie .eq. 'REEL') then
                                    vale = dble(zc(jcesv-1+iad))
                                else if (partie.eq.'IMAG') then
                                    vale = dimag(zc(jcesv-1+iad))
                                endif
                                if (abs(vale) .le. 1.d-99) vale = 0.d0
                                val2(k) = vale
                                iadmax=iad
                            else
                                vale = 0.d0
                                val2(k) = vale
                            endif
26                      continue
                    endif
                    write(ifi,1010) val2(1),val2(4),val2(5),val2(4),&
                    val2(2), val2(6),val2(5),val2(6),val2(3)
                    goto 25
23              continue
                if (itrou .eq. 0) then
                    call u2mess('F', 'PREPOST2_58')
                endif
25              continue
24          continue
        endif
11  end do
!
    call jedema()
!
    1010 format(1p,9(e15.7e3))
!
end subroutine

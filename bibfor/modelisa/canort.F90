subroutine canort(noma, nbma, listi, listk, ndim,&
                  nbno, nuno, l)
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
! aslint: disable=W1501
    implicit none
    include 'jeveux.h'
!
    include 'asterc/r8rddg.h'
    include 'asterfort/cncinv.h'
    include 'asterfort/codree.h'
    include 'asterfort/dffno.h'
    include 'asterfort/jecreo.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeecra.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenonu.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/ltnotb.h'
    include 'asterfort/norlin.h'
    include 'asterfort/provec.h'
    include 'asterfort/tbliva.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/wkvect.h'
    integer :: nbma, listi(*), ndim, nbno, nuno(*), l
    character(len=8) :: noma, listk(*)
!
!     BUT: CALCULER LES NORMALES AUX NOEUDS D'UNE LISTE DE MAILLES
!                   ET LES TANGENTES
! ARGUMENTS D'ENTREE:
!      NOMA : NOM DU MAILLAGE
!      NBMA : NOMBRE DE MAILLES DU MAILLAGE DANS LA LISTE.
!              SI >0 LA LISTE EST NUMEROTEE ==> LISTI
!              SI <0 LA LISTE EST NOMMEE    ==> LISTK
!      NDIM : DIMENSION DU PROBLEME
!      NBNO : NOMBRE DE NOEUDS DANS LA LISTE DE MAILLES.
!             = NOMBRE DE MAILLES SUPPLEMENTAIRES.
!      NUNO : LISTE DES NUMEROS DE NOEUDS DE LA LISTE DE MAILLES
!      L    : =1 ==> CALCUL DE LA NORMALE (2D ET 3D)
!             =2 ==> CALCUL DE LA TANGENTE (2D )
! OBJETS JEVEUX CREES
!     &&CANORT.NORMALE : NORMALES MOYENNEES AUX NOEUDS (2 EN 2D,3 EN 3D)
!     &&CANORT.TANGENT : TANGENTES AUX NOEUDS (2 EN 2D)
!
! ROUTINES APPELEES:
!
!
!
    integer :: dimcoo, i, ifonc, ibid, iret, jnorm, isom, in
    integer :: idobj2, jcoor, iatyma, m, jcoode, ij, ino
    integer :: n, nocc, nno, nnos, nbpar, nnn
    integer :: iinver, imail, numail, ityp, jdes, nn, numno, lino(9)
    real(kind=8) :: coor(3, 9), a, b, c, pvec(3), norme, r8b
    complex(kind=8) :: c16b
    character(len=8) :: kangl, k8b, knumai
    character(len=8) :: mk, nomtyp, nomnoe, k8bid
    character(len=19) :: nomt19
    character(len=24) :: nomobj, nomob2, coninv, para
    character(len=24) :: valk(2)
    character(len=1) :: k1b
    real(kind=8) :: dfse2(4), dfse3(9), armin, prec
    real(kind=8) :: dftr3(18), dftr6(72), dftr7(98)
    real(kind=8) :: dfqu4(32), dfqu8(128), dfqu9(162)
    real(kind=8) :: eksix, eksiy, eksiz, eetax, eetay, eetaz
    real(kind=8) :: vnorm, cosvec, sinvec, angl, atan2
!
!
    call jemarq()
!
!     RECUPERATION DES FONCTIONS DE FORMES POUR TOUS LES
!     TYPES D ELEMENTS SUSCEPTIBLES D ETRE PRESENT
!
    call dffno('SE2', ibid, nno, nnos, dfse2)
    call dffno('SE3', ibid, nno, nnos, dfse3)
    call dffno('TR3', ibid, nno, nnos, dftr3)
    call dffno('TR6', ibid, nno, nnos, dftr6)
    call dffno('TR7', ibid, nno, nnos, dftr7)
    call dffno('QU4', ibid, nno, nnos, dfqu4)
    call dffno('QU8', ibid, nno, nnos, dfqu8)
    call dffno('QU9', ibid, nno, nnos, dfqu9)
    coninv='&&CANORT.CONINV'
!
    if (l .eq. 1) nomobj = '&&CANORT.NORMALE'
    if (l .eq. 2) nomobj = '&&CANORT.TANGENT'
    call jeexin(nomobj, iret)
    if (iret .ne. 0) call jedetr(nomobj)
    call jecreo(nomobj, 'V V R')
    call jeecra(nomobj, 'LONMAX', ndim*nbno, ' ')
    call jeveuo(nomobj, 'E', jnorm)
!
    call jeveuo(noma//'.COORDO    .VALE', 'L', jcoor)
    call jeveuo(noma//'.TYPMAIL', 'L', iatyma)
!
! --- RECUPERATION DE L'ARETE MINIMUM DU MAILLAGE
!
    call jeexin(noma//'           .LTNT', iret)
    if (iret .ne. 0) then
        call ltnotb(noma, 'CARA_GEOM', nomt19)
        nbpar = 0
        para = 'AR_MIN                  '
        call tbliva(nomt19, nbpar, ' ', ibid, r8b,&
                    c16b, k8bid, k8bid, r8b, para,&
                    k8bid, ibid, armin, c16b, k8bid,&
                    iret)
        if (iret .ne. 0) call u2mess('F', 'MODELISA2_13')
        prec = armin*1.d-06
    else
        prec = 1.d-10
    endif
!
!     TRANSFORMATION DE LA LISTE DE NOM DE MAILLES EN LISTE DE NUMERO
!     DE MAILLE ( POUR PASSAGE DANS CNCINV )
    if (nbma .lt. 0) then
        do 5 m = 1, abs(nbma)
            mk=listk(m)
            call jenonu(jexnom(noma//'.NOMMAI', mk), listi(m))
 5      continue
    endif
!
!     RECUPERATION DE LA CONNECTIVITE INVERSE
    call cncinv(noma, listi, abs(nbma), 'V', coninv)
!
    nomob2 = '&&CANORT.VECTEUR'
    call jeexin(nomob2, iret)
    if (iret .ne. 0) call jedetr(nomob2)
    isom = 0
    do 1 i = 1, nbno
        call jelira(jexnum(coninv, nuno(i)), 'LONMAX', nnn, k8b)
        isom = isom + nnn
 1  end do
!
    call wkvect(nomob2, 'V V R', ndim*isom, idobj2)
!
    call jeveuo(noma//'.COORDO    .DESC', 'L', jcoode)
!
    ij=0
!     BOUCLE SUR TOUS LES NOEUDS CONCERNES
    do 10 ino = 1, nbno
        numno=nuno(ino)
        call jelira(jexnum(coninv, numno), 'LONMAX', nnn, k8b)
        call jeveuo(jexnum(coninv, numno), 'L', iinver)
!
!    BOUCLE SUR TOUTES LES MAILLES CONNECTEES AU NOEUD ACTUEL
        do 20 imail = 1, nnn
!
!           NUMERO ABSOLUE DE LA MAILLE
!
            numail=listi(zi(iinver-1+imail))
            ityp=zi(iatyma-1+numail)
            call jenuno(jexnum('&CATA.TM.NOMTM', ityp), nomtyp)
            call jeveuo(jexnum(noma//'.CONNEX', numail), 'L', jdes)
            call jelira(jexnum(noma//'.CONNEX', numail), 'LONMAX', nn, k1b)
            if (ndim .eq. 2 .and. nomtyp(1:4) .eq. 'SEG2') then
                dimcoo = -zi(jcoode-1+2)
                lino(1)=zi(jdes-1+1)
                lino(2)=zi(jdes-1+2)
                coor(1,1)=zr(jcoor-1+dimcoo*(lino(1)-1)+1)
                coor(2,1)=zr(jcoor-1+dimcoo*(lino(1)-1)+2)
                coor(1,2)=zr(jcoor-1+dimcoo*(lino(2)-1)+1)
                coor(2,2)=zr(jcoor-1+dimcoo*(lino(2)-1)+2)
                eksix=coor(1,1)*dfse2(1)+coor(1,2)*dfse2(2)
                eksiy=coor(2,1)*dfse2(1)+coor(2,2)*dfse2(2)
                if (l .eq. 2) then
                    norme=sqrt(eksix**2+eksiy**2)
                    if (norme .gt. prec) then
                        a=eksix/norme
                        b=eksiy/norme
                    else
                        call jenuno(jexnum(noma//'.NOMMAI', numail), knumai)
                        call u2mesk('F', 'MODELISA3_23', 1, knumai)
                    endif
                else if (l.eq.1) then
                    norme=sqrt(eksix**2+eksiy**2)
                    if (norme .gt. prec) then
                        a=eksiy/norme
                        b=-eksix/norme
                    else
                        call jenuno(jexnum(noma//'.NOMMAI', numail), knumai)
                        call u2mesk('F', 'MODELISA3_24', 1, knumai)
                    endif
                endif
                zr(jnorm-1+2*(ino-1)+1)=zr(jnorm-1+2*(ino-1)+1)&
                +a/nnn
                zr(jnorm-1+2*(ino-1)+2)=zr(jnorm-1+2*(ino-1)+2)&
                +b/nnn
                ij=ij+1
                zr(idobj2-1+2*(ij-1)+1) = a
                zr(idobj2-1+2*(ij-1)+2) = b
            else if (ndim.eq.2.and.nomtyp(1:4).eq.'SEG3') then
                do 30 i = 1, nn
                    dimcoo = -zi(jcoode-1+2)
                    lino(i)=zi(jdes-1+i)
                    coor(1,i)=zr(jcoor-1+dimcoo*(lino(i)-1)+1)
                    coor(2,i)=zr(jcoor-1+dimcoo*(lino(i)-1)+2)
                    coor(3,i)=0.d0
                    if (numno .eq. lino(i)) in=i
30              continue
                eksix=0.d0
                eksiy=0.d0
!              CALCUL DU  VECTEUR TANGENT VIA LES FONCTIONS DE FORMES
                do 35 ifonc = 1, nn
                    eksix=eksix+coor(1,ifonc)*dfse3((in-1)*nn+ifonc)
                    eksiy=eksiy+coor(2,ifonc)*dfse3((in-1)*nn+ifonc)
35              continue
!              ON S INTERESSE AU VECTEUR TANGENT
                if (l .eq. 2) then
                    norme=sqrt(eksix**2+eksiy**2)
                    if (norme .gt. prec) then
                        a=eksix/norme
                        b=eksiy/norme
                    else
                        call norlin('SE3', 2, knumai, coor, dfse2,&
                                    in, prec, a, b, c)
                    endif
!
!              ON S INTERESSE AU VECTEUR NORMAL
                else if (l.eq.1) then
                    norme=sqrt(eksix**2+eksiy**2)
                    if (norme .gt. prec) then
                        a=eksiy/norme
                        b=-eksix/norme
                    else
                        call norlin('SE3', 1, knumai, coor, dfse2,&
                                    in, prec, a, b, c)
                    endif
                endif
                zr(jnorm-1+2*(ino-1)+1)=zr(jnorm-1+2*(ino-1)+1)&
                +a/nnn
                zr(jnorm-1+2*(ino-1)+2)=zr(jnorm-1+2*(ino-1)+2)&
                +b/nnn
                ij=ij+1
                zr(idobj2-1+2*(ij-1)+1) = a
                zr(idobj2-1+2*(ij-1)+2) = b
            else if (ndim.eq.3.and.nomtyp(1:3).eq.'SEG') then
                call u2mess('F', 'MODELISA3_25')
!
            else if (ndim.eq.3.and.nomtyp(1:5).eq.'QUAD4') then
                do 40 i = 1, nn
                    lino(i)=zi(jdes-1+i)
                    coor(1,i)=zr(jcoor-1+3*(lino(i)-1)+1)
                    coor(2,i)=zr(jcoor-1+3*(lino(i)-1)+2)
                    coor(3,i)=zr(jcoor-1+3*(lino(i)-1)+3)
                    if (numno .eq. lino(i)) in=i
40              continue
                eksix=0.d0
                eksiy=0.d0
                eksiz=0.d0
                eetax=0.d0
                eetay=0.d0
                eetaz=0.d0
!
!              CALCUL DES DEUX VECTEURS TANGENTS
                do 45 ifonc = 1, nn
                    eksix=eksix+coor(1,ifonc)*dfqu4((in-1)*nn*2+ifonc)
                    eksiy=eksiy+coor(2,ifonc)*dfqu4((in-1)*nn*2+ifonc)
                    eksiz=eksiz+coor(3,ifonc)*dfqu4((in-1)*nn*2+ifonc)
!
                    eetax=eetax+coor(1,ifonc)*dfqu4((in-1)*nn*2+nn+&
                    ifonc)
                    eetay=eetay+coor(2,ifonc)*dfqu4((in-1)*nn*2+nn+&
                    ifonc)
                    eetaz=eetaz+coor(3,ifonc)*dfqu4((in-1)*nn*2+nn+&
                    ifonc)
45              continue
!
!              CALCUL DU VECTEUR NORMAL ET NORMALISATION
                a=eksiy*eetaz-eksiz*eetay
                b=eksiz*eetax-eksix*eetaz
                c=eksix*eetay-eksiy*eetax
                norme=sqrt(a*a+b*b+c*c)
                if (norme .gt. prec) then
                    a=a/norme
                    b=b/norme
                    c=c/norme
                else
                    call jenuno(jexnum(noma//'.NOMMAI', numail), knumai)
                    call u2mesk('F', 'MODELISA3_26', 1, knumai)
                endif
!              ON FAIT LA MOYENNE SUR TOUTES LES MAILLES DES NORMALES
!              RELATIVES A UN NOEUD
                zr(jnorm-1+3*(ino-1)+1)=zr(jnorm-1+3*(ino-1)+1)&
                +a/nnn
                zr(jnorm-1+3*(ino-1)+2)=zr(jnorm-1+3*(ino-1)+2)&
                +b/nnn
                zr(jnorm-1+3*(ino-1)+3)=zr(jnorm-1+3*(ino-1)+3)&
                +c/nnn
                ij=ij+1
!              ON STOCHE DANS L OBJET IDOBJ2 TOUTES LES NORMALES POUR
!              UNE VERIFICATION ULTERIEURE
                zr(idobj2-1+3*(ij-1)+1) = a
                zr(idobj2-1+3*(ij-1)+2) = b
                zr(idobj2-1+3*(ij-1)+3) = c
            else if (ndim.eq.3.and.nomtyp(1:5).eq.'QUAD8') then
                do 50 i = 1, nn
                    lino(i)=zi(jdes-1+i)
                    coor(1,i)=zr(jcoor-1+3*(lino(i)-1)+1)
                    coor(2,i)=zr(jcoor-1+3*(lino(i)-1)+2)
                    coor(3,i)=zr(jcoor-1+3*(lino(i)-1)+3)
                    if (numno .eq. lino(i)) in=i
50              continue
                eksix=0.d0
                eksiy=0.d0
                eksiz=0.d0
                eetax=0.d0
                eetay=0.d0
                eetaz=0.d0
!              CALCUL DES DEUX VECTEURS TANGENTS
                do 55 ifonc = 1, nn
!
                    eksix=eksix+coor(1,ifonc)*dfqu8((in-1)*nn*2+ifonc)
                    eksiy=eksiy+coor(2,ifonc)*dfqu8((in-1)*nn*2+ifonc)
                    eksiz=eksiz+coor(3,ifonc)*dfqu8((in-1)*nn*2+ifonc)
!
                    eetax=eetax+coor(1,ifonc)*dfqu8((in-1)*nn*2+nn+&
                    ifonc)
                    eetay=eetay+coor(2,ifonc)*dfqu8((in-1)*nn*2+nn+&
                    ifonc)
                    eetaz=eetaz+coor(3,ifonc)*dfqu8((in-1)*nn*2+nn+&
                    ifonc)
55              continue
!              CALCUL DU VECTEUR NORMAL ET NORMALISATION
                a=eksiy*eetaz-eksiz*eetay
                b=eksiz*eetax-eksix*eetaz
                c=eksix*eetay-eksiy*eetax
                norme=sqrt(a*a+b*b+c*c)
                if (norme .gt. prec) then
                    a=a/norme
                    b=b/norme
                    c=c/norme
                else
                    call norlin('QU8', 0, knumai, coor, dfqu4,&
                                in, prec, a, b, c)
                endif
!              ON FAIT LA MOYENNE SUR TOUTES LES MAILLES DES NORMALES
!              RELATIVES A UN NOEUD
                zr(jnorm-1+3*(ino-1)+1)=zr(jnorm-1+3*(ino-1)+1)&
                +a/nnn
                zr(jnorm-1+3*(ino-1)+2)=zr(jnorm-1+3*(ino-1)+2)&
                +b/nnn
                zr(jnorm-1+3*(ino-1)+3)=zr(jnorm-1+3*(ino-1)+3)&
                +c/nnn
                ij=ij+1
!              ON STOCHE DANS L OBJET IDOBJ2 TOUTES LES NORMALES POUR
!              UNE VERIFICATION ULTERIEURE
                zr(idobj2-1+3*(ij-1)+1) = a
                zr(idobj2-1+3*(ij-1)+2) = b
                zr(idobj2-1+3*(ij-1)+3) = c
            else if (ndim.eq.3.and.nomtyp(1:5).eq.'QUAD9') then
                do 60 i = 1, nn
                    lino(i)=zi(jdes-1+i)
                    coor(1,i)=zr(jcoor-1+3*(lino(i)-1)+1)
                    coor(2,i)=zr(jcoor-1+3*(lino(i)-1)+2)
                    coor(3,i)=zr(jcoor-1+3*(lino(i)-1)+3)
                    if (numno .eq. lino(i)) in=i
60              continue
                eksix=0.d0
                eksiy=0.d0
                eksiz=0.d0
                eetax=0.d0
                eetay=0.d0
                eetaz=0.d0
!              CALCUL DES DEUX VECTEURS TANGENTS
                do 65 ifonc = 1, nn
                    eksix=eksix+coor(1,ifonc)*dfqu9((in-1)*nn*2+ifonc)
                    eksiy=eksiy+coor(2,ifonc)*dfqu9((in-1)*nn*2+ifonc)
                    eksiz=eksiz+coor(3,ifonc)*dfqu9((in-1)*nn*2+ifonc)
!
                    eetax=eetax+coor(1,ifonc)*dfqu9((in-1)*nn*2+nn+&
                    ifonc)
                    eetay=eetay+coor(2,ifonc)*dfqu9((in-1)*nn*2+nn+&
                    ifonc)
                    eetaz=eetaz+coor(3,ifonc)*dfqu9((in-1)*nn*2+nn+&
                    ifonc)
65              continue
!              CALCUL DU VECTEUR NORMAL ET NORMALISATION
                a=eksiy*eetaz-eksiz*eetay
                b=eksiz*eetax-eksix*eetaz
                c=eksix*eetay-eksiy*eetax
                norme=sqrt(a*a+b*b+c*c)
                if (norme .gt. prec) then
                    a=a/norme
                    b=b/norme
                    c=c/norme
                else
                    call jenuno(jexnum(noma//'.NOMMAI', numail), knumai)
                    call u2mesk('F', 'MODELISA3_26', 1, knumai)
                endif
!              ON FAIT LA MOYENNE SUR TOUTES LES MAILLES DES NORMALES
!              RELATIVES A UN NOEUD
                zr(jnorm-1+3*(ino-1)+1)=zr(jnorm-1+3*(ino-1)+1)&
                +a/nnn
                zr(jnorm-1+3*(ino-1)+2)=zr(jnorm-1+3*(ino-1)+2)&
                +b/nnn
                zr(jnorm-1+3*(ino-1)+3)=zr(jnorm-1+3*(ino-1)+3)&
                +c/nnn
                ij=ij+1
!              ON STOCHE DANS L OBJET IDOBJ2 TOUTES LES NORMALES POUR
!              UNE VERIFICATION ULTERIEURE
                zr(idobj2-1+3*(ij-1)+1) = a
                zr(idobj2-1+3*(ij-1)+2) = b
                zr(idobj2-1+3*(ij-1)+3) = c
            else if (ndim.eq.3.and.nomtyp(1:5).eq.'TRIA3') then
                do 70 i = 1, nn
                    lino(i)=zi(jdes-1+i)
                    coor(1,i)=zr(jcoor-1+3*(lino(i)-1)+1)
                    coor(2,i)=zr(jcoor-1+3*(lino(i)-1)+2)
                    coor(3,i)=zr(jcoor-1+3*(lino(i)-1)+3)
                    if (numno .eq. lino(i)) in=i
70              continue
!              CALCUL DES DEUX VECTEURS TANGENTS
                eksix=0.d0
                eksiy=0.d0
                eksiz=0.d0
                eetax=0.d0
                eetay=0.d0
                eetaz=0.d0
                do 75 ifonc = 1, nn
                    eksix=eksix+coor(1,ifonc)*dftr3((in-1)*nn*2+ifonc)
                    eksiy=eksiy+coor(2,ifonc)*dftr3((in-1)*nn*2+ifonc)
                    eksiz=eksiz+coor(3,ifonc)*dftr3((in-1)*nn*2+ifonc)
!
                    eetax=eetax+coor(1,ifonc)*dftr3((in-1)*nn*2+nn+&
                    ifonc)
                    eetay=eetay+coor(2,ifonc)*dftr3((in-1)*nn*2+nn+&
                    ifonc)
                    eetaz=eetaz+coor(3,ifonc)*dftr3((in-1)*nn*2+nn+&
                    ifonc)
75              continue
!              CALCUL DU VECTEUR NORMAL ET NORMALISATION
                a=eksiy*eetaz-eksiz*eetay
                b=eksiz*eetax-eksix*eetaz
                c=eksix*eetay-eksiy*eetax
                norme=sqrt(a*a+b*b+c*c)
                if (norme .gt. prec) then
                    a=a/norme
                    b=b/norme
                    c=c/norme
                else
                    call jenuno(jexnum(noma//'.NOMMAI', numail), knumai)
                    call u2mesk('F', 'MODELISA3_26', 1, knumai)
                endif
!              ON FAIT LA MOYENNE SUR TOUTES LES MAILLES DES NORMALES
!              RELATIVES A UN NOEUD
                zr(jnorm-1+3*(ino-1)+1)=zr(jnorm-1+3*(ino-1)+1)&
                +a/nnn
                zr(jnorm-1+3*(ino-1)+2)=zr(jnorm-1+3*(ino-1)+2)&
                +b/nnn
                zr(jnorm-1+3*(ino-1)+3)=zr(jnorm-1+3*(ino-1)+3)&
                +c/nnn
                ij=ij+1
!              ON STOCHE DANS L OBJET IDOBJ2 TOUTES LES NORMALES POUR
!              UNE VERIFICATION ULTERIEURE
                zr(idobj2-1+3*(ij-1)+1) = a
                zr(idobj2-1+3*(ij-1)+2) = b
                zr(idobj2-1+3*(ij-1)+3) = c
            else if (ndim.eq.3.and.nomtyp(1:5).eq.'TRIA6') then
                do 90 i = 1, nn
                    lino(i)=zi(jdes-1+i)
                    coor(1,i)=zr(jcoor-1+3*(lino(i)-1)+1)
                    coor(2,i)=zr(jcoor-1+3*(lino(i)-1)+2)
                    coor(3,i)=zr(jcoor-1+3*(lino(i)-1)+3)
                    if (numno .eq. lino(i)) in=i
90              continue
!              CALCUL DES DEUX VECTEURS TANGENTS
                eksix=0.d0
                eksiy=0.d0
                eksiz=0.d0
                eetax=0.d0
                eetay=0.d0
                eetaz=0.d0
                do 95 ifonc = 1, nn
                    eksix=eksix+coor(1,ifonc)*dftr6((in-1)*nn*2+ifonc)
                    eksiy=eksiy+coor(2,ifonc)*dftr6((in-1)*nn*2+ifonc)
                    eksiz=eksiz+coor(3,ifonc)*dftr6((in-1)*nn*2+ifonc)
!
                    eetax=eetax+coor(1,ifonc)*dftr6((in-1)*nn*2+nn+&
                    ifonc)
                    eetay=eetay+coor(2,ifonc)*dftr6((in-1)*nn*2+nn+&
                    ifonc)
                    eetaz=eetaz+coor(3,ifonc)*dftr6((in-1)*nn*2+nn+&
                    ifonc)
95              continue
!              CALCUL DU VECTEUR NORMAL ET NORMALISATION
                a=eksiy*eetaz-eksiz*eetay
                b=eksiz*eetax-eksix*eetaz
                c=eksix*eetay-eksiy*eetax
                norme=sqrt(a*a+b*b+c*c)
                if (norme .gt. prec) then
                    a=a/norme
                    b=b/norme
                    c=c/norme
                else
                    call norlin('TR6', 0, knumai, coor, dftr3,&
                                in, prec, a, b, c)
                endif
!              ON FAIT LA MOYENNE SUR TOUTES LES MAILLES DES NORMALES
!              RELATIVES A UN NOEUD
                zr(jnorm-1+3*(ino-1)+1)=zr(jnorm-1+3*(ino-1)+1)&
                +a/nnn
                zr(jnorm-1+3*(ino-1)+2)=zr(jnorm-1+3*(ino-1)+2)&
                +b/nnn
                zr(jnorm-1+3*(ino-1)+3)=zr(jnorm-1+3*(ino-1)+3)&
                +c/nnn
                ij=ij+1
!              ON STOCHE DANS L OBJET IDOBJ2 TOUTES LES NORMALES POUR
!              UNE VERIFICATION ULTERIEURE
                zr(idobj2-1+3*(ij-1)+1) = a
                zr(idobj2-1+3*(ij-1)+2) = b
                zr(idobj2-1+3*(ij-1)+3) = c
            else if (ndim.eq.3.and.nomtyp(1:5).eq.'TRIA7') then
                do 100 i = 1, nn
                    lino(i)=zi(jdes-1+i)
                    coor(1,i)=zr(jcoor-1+3*(lino(i)-1)+1)
                    coor(2,i)=zr(jcoor-1+3*(lino(i)-1)+2)
                    coor(3,i)=zr(jcoor-1+3*(lino(i)-1)+3)
                    if (numno .eq. lino(i)) in=i
100              continue
!              CALCUL DES DEUX VECTEURS TANGENTS
                eksix=0.d0
                eksiy=0.d0
                eksiz=0.d0
                eetax=0.d0
                eetay=0.d0
                eetaz=0.d0
                do 105 ifonc = 1, nn
                    eksix=eksix+coor(1,ifonc)*dftr7((in-1)*nn*2+ifonc)
                    eksiy=eksiy+coor(2,ifonc)*dftr7((in-1)*nn*2+ifonc)
                    eksiz=eksiz+coor(3,ifonc)*dftr7((in-1)*nn*2+ifonc)
!
                    eetax=eetax+coor(1,ifonc)*dftr7((in-1)*nn*2+nn+&
                    ifonc)
                    eetay=eetay+coor(2,ifonc)*dftr7((in-1)*nn*2+nn+&
                    ifonc)
                    eetaz=eetaz+coor(3,ifonc)*dftr7((in-1)*nn*2+nn+&
                    ifonc)
105              continue
!              CALCUL DU VECTEUR NORMAL ET NORMALISATION
                a=eksiy*eetaz-eksiz*eetay
                b=eksiz*eetax-eksix*eetaz
                c=eksix*eetay-eksiy*eetax
                norme=sqrt(a*a+b*b+c*c)
                if (norme .gt. prec) then
                    a=a/norme
                    b=b/norme
                    c=c/norme
                else
                    call jenuno(jexnum(noma//'.NOMMAI', numail), knumai)
                    call u2mesk('F', 'MODELISA3_26', 1, knumai)
                endif
!              ON FAIT LA MOYENNE SUR TOUTES LES MAILLES DES NORMALES
!              RELATIVES A UN NOEUD
                zr(jnorm-1+3*(ino-1)+1)=zr(jnorm-1+3*(ino-1)+1)&
                +a/nnn
                zr(jnorm-1+3*(ino-1)+2)=zr(jnorm-1+3*(ino-1)+2)&
                +b/nnn
                zr(jnorm-1+3*(ino-1)+3)=zr(jnorm-1+3*(ino-1)+3)&
                +c/nnn
                ij=ij+1
!              ON STOCHE DANS L OBJET IDOBJ2 TOUTES LES NORMALES POUR
!              UNE VERIFICATION ULTERIEURE
                zr(idobj2-1+3*(ij-1)+1) = a
                zr(idobj2-1+3*(ij-1)+2) = b
                zr(idobj2-1+3*(ij-1)+3) = c
            else
                call u2mess('F', 'MODELISA3_27')
            endif
20      continue
10  end do
!
!
    ij = 0
    do 2 n = 1, nbno
        ino = nuno(n)
        call jelira(jexnum(coninv, ino), 'LONMAX', nocc, k8b)
        if (ndim .eq. 2) then
            vnorm = zr(&
                    jnorm-1+2*(n-1)+1)*zr(jnorm-1+2*(n-1)+1) + zr(jnorm-1+2*(n-1)+2)*zr(jnorm-1+2&
                    &*(n-1)+2&
                    )
            vnorm = sqrt(vnorm)
            if (vnorm .lt. 1.0d-2) then
                call jenuno(jexnum(noma//'.NOMNOE', ino), nomnoe)
                call u2mesk('F', 'MODELISA3_28', 1, nomnoe)
            endif
            zr(jnorm-1+2*(n-1)+1)=zr(jnorm-1+2*(n-1)+1)/vnorm
            zr(jnorm-1+2*(n-1)+2)=zr(jnorm-1+2*(n-1)+2)/vnorm
            do 7 i = 1, nocc
                ij = ij + 1
                cosvec = zr(&
                         jnorm-1+2*(n-1)+1)*zr(idobj2-1+2*(ij-1)+1) + zr(jnorm-1+2*(n-1)+2)*zr(id&
                         &obj2-1+2*(ij-1)+2&
                         )
                sinvec = zr(&
                         jnorm-1+2*(n-1)+1)*zr(idobj2-1+2*(ij-1)+2) - zr(jnorm-1+2*(n-1)+2)*zr(id&
                         &obj2-1+2*(ij-1)+1&
                         )
                angl = r8rddg()*atan2(sinvec,cosvec)
                if (abs(angl) .gt. 10.0d0) then
                    call jenuno(jexnum(noma//'.NOMNOE', ino), nomnoe)
                    call codree(abs(angl), 'G', kangl)
                    valk(1) = nomnoe
                    valk(2) = kangl
                    call u2mesk('A', 'MODELISA3_29', 2, valk)
                endif
 7          continue
        else if (ndim.eq.3) then
            vnorm = zr(&
                    jnorm-1+3*(n-1)+1)*zr(jnorm-1+3*(n-1)+1) + zr(jnorm-1+3*(n-1)+2)*zr(jnorm-1+3&
                    &*(n-1)+2) + zr(jnorm- 1+3*(n-1)+3)*zr(jnorm-1+3*(n-1)+3&
                    )
            vnorm = sqrt(vnorm)
            if (vnorm .lt. 1.0d-2) then
                call jenuno(jexnum(noma//'.NOMNOE', ino), nomnoe)
                call u2mesk('F', 'MODELISA3_30', 1, nomnoe)
            endif
            zr(jnorm-1+3*(n-1)+1)=zr(jnorm-1+3*(n-1)+1)/vnorm
            zr(jnorm-1+3*(n-1)+2)=zr(jnorm-1+3*(n-1)+2)/vnorm
            zr(jnorm-1+3*(n-1)+3)=zr(jnorm-1+3*(n-1)+3)/vnorm
            do 8 i = 1, nocc
                ij = ij + 1
                cosvec = zr(&
                         jnorm-1+3*(n-1)+1)*zr(idobj2-1+3*(ij-1)+1) + zr(jnorm-1+3*(n-1)+2)*zr(id&
                         &obj2-1+3*(ij-1)+2) + zr(jnorm-1+3*(n-1)+3)*zr(idobj2-1+3*(ij-1)+3&
                         )
                call provec(zr(jnorm-1+3*(n-1)+1), zr(idobj2-1+3*(ij- 1)+1), pvec)
                sinvec = pvec(1)*pvec(1) + pvec(2)*pvec(2) + pvec(3)* pvec(3)
                sinvec = sqrt(sinvec)
                angl = r8rddg()*atan2(sinvec,cosvec)
                if (abs(angl) .gt. 10.0d0) then
                    call jenuno(jexnum(noma//'.NOMNOE', ino), nomnoe)
                    call codree(abs(angl), 'G', kangl)
                    valk(1) = nomnoe
                    valk(2) = kangl
                    call u2mesk('A', 'MODELISA3_31', 2, valk)
                endif
 8          continue
        endif
 2  end do
!
    call jedetr(nomob2)
    call jedetr(coninv)
    call jedema()
end subroutine

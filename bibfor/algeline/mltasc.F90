subroutine mltasc(nbloc, lgbloc, adinit, nommat, lonmat,&
                  factol, factou, typsym)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
! COMPIL PARAL
! VERSION COMPLEXE DE MLTASA
    implicit none
    include 'jeveux.h'
!
    include 'asterfort/jecrec.h'
    include 'asterfort/jecroc.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeecra.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelibe.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnum.h'
    integer :: nbloc, lgbloc(*), lonmat, adinit(lonmat), typsym
    character(len=24) :: factol, factou, valm
    character(len=*) :: nommat
    integer :: fin, deb, mati, mats, adprov
    integer :: ibid, ip, irefac, lgblib
!===============================================================
!     ASSEMBLAGE DE LA MATRICE INITIALE DANS LA MATRICE FACTOR
!     VERSION ASTER
!     ON CONSIDERE LA MATRICE INITIALE SYMETRIQUE INFERIEURE
!     --------------             PAR LIGNES
!     ----------
!     VERSION NON SYMETRIQUE
!=============================================================
    character(len=8) :: base
    integer :: i, i1, ib, ifacl, ifacu, code
    data valm/'                   .VALM'/
    valm(1:19) = nommat
    call jemarq()
    if (typsym .eq. 1) then
        ip = 1
        call jeveuo(jexnum(valm, ip), 'L', mati)
        do 10 i = 1, lonmat
            if (adinit(i) .le. 0) adinit(i) = - adinit(i)
10      continue
    else
        ip = 1
        call jeveuo(jexnum(valm, ip), 'L', mats)
        ip = 2
        call jeveuo(jexnum(valm, ip), 'L', mati)
    endif
!===================================================================
!     CREATION D'UNE COLLECTION DISPERSEE
!
!--   DEVANT RECREER LA COLLECTION FACTOR, ON LA DETRUIT SI ELLE EXISTE
!--   DEJA
!
    call jeexin(factol, irefac)
    if (irefac .gt. 0) then
        call jedetr(factol)
    endif
    call jelira(jexnum(valm, ip), 'CLAS', ibid, base)
    call jecrec(factol, base(1:1)//' V C ', 'NU', 'DISPERSE', 'VARIABLE',&
                nbloc)
    do 50 ib = 1, nbloc
        call jecroc(jexnum(factol, ib))
        lgblib = lgbloc(ib)
        call jeecra(jexnum(factol, ib), 'LONMAX', lgblib, ' ')
50  end do
    fin = 0
    if (typsym .eq. 0) then
!        CAS NON-SYMETRIQUE
        call jeexin(factou, irefac)
        if (irefac .gt. 0) then
            call jedetr(factou)
        endif
        call jelira(jexnum(valm, ip), 'CLAS', ibid, base)
        call jecrec(factou, base(1:1)//' V C ', 'NU', 'DISPERSE', 'VARIABLE',&
                    nbloc)
        do 51 ib = 1, nbloc
            call jecroc(jexnum(factou, ib))
            lgblib = lgbloc(ib)
            call jeecra(jexnum(factou, ib), 'LONMAX', lgblib, ' ')
51      continue
!
        do 130 ib = 1, nbloc
            call jeveuo(jexnum(factol, ib), 'E', ifacl)
            call jeveuo(jexnum(factou, ib), 'E', ifacu)
            do 110 i = 1, lgbloc(ib)
                zc(ifacl+i-1) = 0.d0
                zc(ifacu+i-1) = 0.d0
110          continue
            deb = fin
            fin = deb + lgbloc(ib)
            deb = deb + 1
!MIC$ DO ALL SHARED (ADINIT, DEB, FIN, IFACL, LONMAT, ZC)
!MIC$*        SHARED (MATI,IFACU,MATS) VECTOR
!MIC$*        PRIVATE (I1,CODE,ADPROV)
!CDIR$ IVDEP
            do 120 i1 = 1, lonmat
                if (adinit(i1) .le. 0) then
                    code =-1
                    adprov = - adinit(i1)
                else
                    code =1
                    adprov = adinit(i1)
                endif
                if (adprov .gt. fin) goto 120
                if (adprov .lt. deb) goto 120
                if (code .gt. 0) then
                    zc(ifacl+adprov-deb) = zc(mati+i1-1)
                    zc(ifacu+adprov-deb) = zc(mats+i1-1)
                else
                    zc(ifacl+adprov-deb) = zc(mats+i1-1)
                    zc(ifacu+adprov-deb) = zc(mati+i1-1)
                endif
120          continue
            call jelibe(jexnum(factol, ib))
            call jelibe(jexnum(factou, ib))
130      continue
        do 140 ip = 1, 2
            call jelibe(jexnum(valm, ip))
140      continue
    else
!     CAS SYMETRIQUE
        fin = 0
        do 135 ib = 1, nbloc
            call jeveuo(jexnum(factol, ib), 'E', ifacl)
            do 115 i = 1, lgbloc(ib)
                zc(ifacl+i-1) = 0.d0
115          continue
!
            deb = fin
            fin = deb + lgbloc(ib)
            deb = deb + 1
!MIC$ DO ALL SHARED (ADINIT, DEB, FIN, IFACL, LONMAT, ZC)
!MIC$*       PRIVATE (I1) SHARED (MATI) VECTOR
!CDIR$ IVDEP
            do 125 i1 = 1, lonmat
                if (adinit(i1) .gt. fin) goto 125
                if (adinit(i1) .lt. deb) goto 125
                zc(ifacl+adinit(i1)-deb) = zc(mati+i1-1)
125          continue
            call jelibe(jexnum(factol, ib))
135      continue
        ip = 1
        call jelibe(jexnum(valm, ip))
    endif
    call jelibe(valm)
    call jedema()
end subroutine

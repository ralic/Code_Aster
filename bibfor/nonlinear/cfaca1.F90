subroutine cfaca1(ndim, nbliac, ajliai, llf, llf1,&
                  llf2, nesmax, defico, resoco, solveu,&
                  lmat, nbliai)
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit     none
    include 'jeveux.h'
!
    include 'asterfort/calatm.h'
    include 'asterfort/cfdisi.h'
    include 'asterfort/cftyli.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jelibe.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/nmrldb.h'
    include 'asterfort/wkvect.h'
    character(len=24) :: defico, resoco
    character(len=19) :: solveu
    integer :: nbliai, nbliac, llf, llf1, llf2
    integer :: ndim, nesmax
    integer :: lmat
    integer :: ajliai
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES DISCRETES - RESOLUTION - A.C-1.AT)
!
! ROUTINE REALISANT LE CALCUL DE A.C-1.AT PAR RESOLUTION DE C.X=A(I)
!     A(I) -> I-EME COLONNE DE A
!     X    -> I-EME COLONNE DE C-1.A
!  LA ROUTINE EST OPTIMISEE PAR TRAITEMENT DES SECONDS MEMBRES PAR BLOCS
!
! ----------------------------------------------------------------------
!
!
! IN  NDIM   : DIMENSION DU PROBLEME
! IN  DEFICO : SD DE DEFINITION DU CONTACT (ISSUE D'AFFE_CHAR_MECA)
! IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
! IN  SOLVEU : SD SOLVEUR
! IN  LMAT   : DESCRIPTEUR DE LA MATR_ASSE DU SYSTEME MECANIQUE
! IN  NBLIAI : NOMBRE DE LIAISONS DE CONTACT POSSIBLES
! IN  NBLIAC : NOMBRE DE LIAISONS ACTIVES
! I/O AJLIAI : INDICE DANS LA LISTE DES LIAISONS ACTIVES DE LA DERNIERE
!              LIAISON CORRECTE DU CALCUL
!              DE LA MATRICE DE CONTACT ACM1AT
! IN  NESMAX : NOMBRE MAX DE NOEUDS ESCLAVES
!              (SERT A DECALER LES POINTEURS POUR LE FROTTEMENT 3D)
! IN  LLF    : NOMBRE DE LIAISONS DE FROTTEMENT (EN 2D)
!              NOMBRE DE LIAISONS DE FROTTEMENT SUIVANT LES DEUX
!               DIRECTIONS SIMULTANEES (EN 3D)
! IN  LLF1   : NOMBRE DE LIAISONS DE FROTTEMENT SUIVANT LA
!               PREMIERE DIRECTION (EN 3D)
! IN  LLF2   : NOMBRE DE LIAISONS DE FROTTEMENT SUIVANT LA
!               SECONDE DIRECTION (EN 3D)
!
!
!
!
    integer :: lg, il
    integer :: lliac, jdecal, nbddl, posit
    integer :: neq, lgbloc, tampon
    integer :: nbsm, npas
    integer :: nrest, ipas, llf3d, kk, iliac, jtmpv, npast
    character(len=19) :: liac, cm1a
    integer :: jliac, jcm1a
    character(len=24) :: appoin, apddl, apcoef, apcofr
    integer :: japptr, japddl, japcoe, japcof
    character(len=24) :: chsecm
    character(len=19) :: cncin0
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- RECUPERATION D'OBJETS JEVEUX
!
    cm1a = resoco(1:14)//'.CM1A'
    appoin = resoco(1:14)//'.APPOIN'
    apddl = resoco(1:14)//'.APDDL'
    liac = resoco(1:14)//'.LIAC'
    apcoef = resoco(1:14)//'.APCOEF'
    apcofr = resoco(1:14)//'.APCOFR'
!
    call jeveuo(appoin, 'L', japptr)
    call jeveuo(apddl, 'L', japddl)
    call jeveuo(liac, 'L', jliac)
    call jeveuo(apcoef, 'L', japcoe)
    if (llf .ne. 0) then
        call jeveuo(apcofr, 'L', japcof)
    else
        if (llf1 .ne. 0) then
            call jeveuo(apcofr, 'L', japcof)
        else
            if (llf2 .ne. 0) then
                call jeveuo(apcofr, 'L', japcof)
            endif
        endif
    endif
!
! --- NOMBRE D'EQUATIONS DU SYSTEME
!
    neq = zi(lmat+2)
!
! --- CHARGEMENT CINEMATIQUE NUL
!
    cncin0 = resoco(1:14)//'.CIN0'
!
! ----------------------------------------------------------------------
! --- PAR METHODE DIRECTE AVEC BLOCS DE SECONDS MEMBRES
! ----------------------------------------------------------------------
!
! --- CALCUL DE LGBLOC
!
    lgbloc = cfdisi(defico,'NB_RESOL')
!
    nbsm = nbliac + llf + llf1 + llf2 - ajliai
    if (lgbloc .gt. nbsm) lgbloc = nbsm
    npas = nbsm / lgbloc
    nrest = nbsm - lgbloc*npas
!
    if (nrest .gt. 0) then
        npast = npas + 1
    else
        npast = npas
    endif
    chsecm='&&CFACA1.TAMPON'
    call wkvect(chsecm, ' V V R ', neq*lgbloc, tampon)
    llf3d = 0
    if (llf .ne. 0) then
        call wkvect('&&CFACA1.VECT', ' V V I ', llf, jtmpv)
    endif
!
    do 10 ipas = 1, npast
        lg = lgbloc
        if (npast .ne. npas .and. (ipas.eq.npast)) lg = nrest
!
        do 40 kk = 1, neq*lg
            zr(tampon-1+kk) = 0.0d0
40      continue
!
        do 20 il = 1, lg
            iliac = lgbloc* (ipas-1) + il + ajliai
            lliac = zi(jliac+iliac-1)
            jdecal = zi(japptr+lliac-1)
            nbddl = zi(japptr+lliac) - zi(japptr+lliac-1)
            call cftyli(resoco, iliac, posit)
            goto (1000, 2000, 3000, 4000) posit
!
! --- AJOUT D'UNE LIAISON DE CONTACT
!
1000          continue
!
! --- CALCUL DE LA COLONNE AT POUR LA LIAISON ACTIVE LLIAC EN CONTACT
!
            call calatm(neq, nbddl, 1.d0, zr(japcoe+jdecal), zi(japddl+ jdecal),&
                        zr(tampon+neq*(il-1)))
            goto 20
!
! --- AJOUT D'UNE LIAISON DE FROTTEMENT SUIVANT LES DEUX DIRECTIONS
!
2000          continue
!
! --- PREMIERE DIRECTION
!
! --- CALCUL DE LA COLONNE AT POUR LA PREMIERE DIRECTION DE FROTTEMENT
!
            llf3d = llf3d + 1
            zi(jtmpv -1 +llf3d) = lliac
            call calatm(neq, nbddl, 1.d0, zr(japcof+jdecal), zi(japddl+ jdecal),&
                        zr(tampon+neq*(il-1)))
            goto 20
!
! --- AJOUT D'UNE LIAISON DE FROTTEMENT SUIVANT LA 1ERE DIRECTION
!
3000          continue
!
! --- CALCUL DE LA COLONNE AT POUR LA PREMIERE DIRECTION DE FROTTEMENT
!
            call calatm(neq, nbddl, 1.d0, zr(japcof+jdecal), zi(japddl+ jdecal),&
                        zr(tampon+neq*(il-1)))
            goto 20
!
! --- AJOUT D'UNE LIAISON DE FROTTEMENT SUIVANT LA 2NDE DIRECTION
!
4000          continue
!
! --- SECONDE DIRECTION
!
! --- CALCUL DE LA COLONNE AT POUR LA SECONDE DIRECTION DE FROTTEMENT
!
            call calatm(neq, nbddl, 1.d0, zr(japcof+jdecal+30*nesmax), zi(japddl+jdecal),&
                        zr(tampon+neq*(il-1)))
20      continue
!
! --- CALCUL DE C-1.AT (EN TENANT COMPTE DES CHARGES CINEMATIQUES)
!
        call nmrldb(solveu, lmat, zr(tampon), lg, cncin0)
!
! --- RECOPIE
!
        do 50 il = 1, lg
            iliac = lgbloc* (ipas-1) + il + ajliai
            lliac = zi(jliac+iliac-1)
            call cftyli(resoco, iliac, posit)
            goto (1100, 2100, 3100, 4100) posit
!
! --- AJOUT D'UNE LIAISON DE CONTACT
!
1100          continue
            call jeveuo(jexnum(cm1a, lliac), 'E', jcm1a)
            do 60 kk = 1, neq
                zr(jcm1a-1+kk) = zr(tampon-1+neq* (il-1)+kk)
60          continue
            call jelibe(jexnum(cm1a, lliac))
            goto 50
!
! --- AJOUT D'UNE LIAISON DE CONTACT
!
2100          continue
            call jeveuo(jexnum(cm1a, lliac+nbliai), 'E', jcm1a)
            do 160 kk = 1, neq
                zr(jcm1a-1+kk) = zr(tampon-1+neq* (il-1)+kk)
160          continue
            call jelibe(jexnum(cm1a, lliac+nbliai))
            goto 50
!
! --- AJOUT D'UNE LIAISON DE CONTACT
!
3100          continue
            call jeveuo(jexnum(cm1a, lliac+nbliai), 'E', jcm1a)
            do 260 kk = 1, neq
                zr(jcm1a-1+kk) = zr(tampon-1+neq* (il-1)+kk)
260          continue
            call jelibe(jexnum(cm1a, lliac+nbliai))
            goto 50
!
! --- AJOUT D'UNE LIAISON DE CONTACT
!
4100          continue
            call jeveuo(jexnum(cm1a, lliac+(ndim-1)*nbliai), 'E', jcm1a)
            do 360 kk = 1, neq
                zr(jcm1a-1+kk) = zr(tampon-1+neq* (il-1)+kk)
360          continue
            call jelibe(jexnum(cm1a, lliac+(ndim-1)*nbliai))
50      continue
10  end do
!
! --- CAS DU FROTTEMENT SUIVANT LA SECONDE DIRECTION EN 3D
!
    if (ndim .eq. 3 .and. llf3d .ne. 0) then
        nbsm = llf3d
        npas = nbsm / lgbloc
        nrest = nbsm - lgbloc*npas
!
        if (nrest .gt. 0) then
            npast = npas + 1
        else
            npast = npas
        endif
!
        do 80 ipas = 1, npast
            lg = lgbloc
            if (npast .ne. npas .and. (ipas.eq.npast)) lg = nrest
!
            do 90 kk = 1, neq*lg
                zr(tampon-1+kk) = 0.0d0
90          continue
!
            do 100 il = 1, lg
                iliac = lgbloc*(ipas-1) + il
                lliac = zi(jtmpv -1 +iliac)
!
! --- AJOUT D'UNE LIAISON DE FROTTEMENT SUIVANT LA 2NDE DIRECTION
!
! --- CALCUL DE LA COLONNE AT POUR LA SECONDE DIRECTION DE FROTTEMENT
!
                jdecal = zi(japptr+lliac-1)
                nbddl = zi(japptr+lliac) - zi(japptr+lliac-1)
                call calatm(neq, nbddl, 1.d0, zr(japcof+jdecal+30*nesmax), zi(japddl+jdecal),&
                            zr(tampon+neq*(il-1)))
100          continue
!
! --- CALCUL DE C-1.AT (EN TENANT COMPTE DES CHARGES CINEMATIQUES)
!
            call nmrldb(solveu, lmat, zr(tampon), lg, cncin0)
!
! --- RECOPIE
            do 110 il = 1, lg
                iliac = lgbloc*(ipas-1) + il
                lliac = zi(jtmpv -1 +iliac)
                call jeveuo(jexnum(cm1a, lliac+(ndim-1)*nbliai), 'E', jcm1a)
                do 120 kk = 1, neq
                    zr(jcm1a-1+kk) = zr(tampon-1+neq* (il-1)+kk)
120              continue
                call jelibe(jexnum(cm1a, lliac+(ndim-1)*nbliai))
110          continue
80      continue
    endif
    ajliai = nbliac + llf + llf1 + llf2
!
    call jedetr('&&CFACA1.TAMPON')
    call jedetr('&&CFACA1.VECT')
!
    call jedema()
!
end subroutine

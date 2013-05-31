subroutine cfaca2(ndim, nbliac, spliai, llf, llf1,&
                  llf2, indfac, nesmax, resoco, lmat,&
                  nbliai, xjvmax)
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
! aslint: disable=W1501
    implicit     none
    include 'jeveux.h'
!
    include 'asterfort/caladu.h'
    include 'asterfort/cftyli.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jelibe.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/wkvect.h'
    integer :: nbliai, nbliac, llf, llf1, llf2
    integer :: spliai, indfac
    integer :: ndim, nesmax
    integer :: lmat
    real(kind=8) :: xjvmax
    character(len=24) :: resoco
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES DISCRETES - RESOLUTION - A.C-1.AT)
!
! CALCUL DE -A.C-1.AT (REDUITE AUX LIAISONS ACTIVES)
! STOCKAGE DE LA MOITIE UNIQUEMENT (PROBLEME SYMETRIQUE)
!
! ----------------------------------------------------------------------
!
!
! IN  NDIM   : DIMENSION DU PROBLEME
! IN  RESOCO : SD DE RESOLUTION DU CONTACT
! IN  LMAT   : DESCRIPTEUR DE LA MATR_ASSE DU SYSTEME MECANIQUE
! IN  NBLIAI : NOMBRE DE LIAISONS DE CONTACT POSSIBLES
! IN  NBLIAC : NOMBRE DE LIAISONS ACTIVES
! I/O XJVMAX : VALEUR DU PIVOT MAX
! I/O SPLIAI : INDICE DANS LA LISTE DES LIAISONS ACTIVES DE LA DERNIERE
!              LIAISON AYANT ETE CALCULEE POUR LE VECTEUR CM1A
! IN  NESMAX : NOMBRE MAX DE NOEUDS ESCLAVES
!              (SERT A DECALER LES POINTEURS POUR LE FROTTEMENT 3D)
! IN  LLF    : NOMBRE DE LIAISONS DE FROTTEMENT (EN 2D)
!              NOMBRE DE LIAISONS DE FROTTEMENT SUIVANT LES DEUX
!               DIRECTIONS SIMULTANEES (EN 3D)
! IN  LLF1   : NOMBRE DE LIAISONS DE FROTTEMENT SUIVANT LA
!               PREMIERE DIRECTION (EN 3D)
! IN  LLF2   : NOMBRE DE LIAISONS DE FROTTEMENT SUIVANT LA
!               SECONDE DIRECTION (EN 3D)
! I/O INDFAC : INDICE DE DEBUT DE LA FACTORISATION
!
!
!
!
    integer :: jdecal
    integer :: nbddl, jva, jvale, deklag, neq, posit
    integer :: iliac, jj, lliac, lljac, ii, dercol, bloc
    integer :: nbbloc
    real(kind=8) :: val
    character(len=2) :: typef0
    character(len=19) :: liac, cm1a, typl
    integer :: jliac, jcm1a, jtypl
    character(len=19) :: stoc
    integer :: jscbl, jscib, jscde
    character(len=19) :: ouvert, macont
    integer :: jouv
    character(len=24) :: appoin, apddl, apcoef, apcofr
    integer :: japptr, japddl, japcoe, japcof
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
    macont = resoco(1:14)//'.MATC'
    typl = resoco(1:14)//'.TYPL'
    stoc = resoco(1:14)//'.SLCS'
!
    call jeveuo(appoin, 'L', japptr)
    call jeveuo(apddl, 'L', japddl)
    call jeveuo(liac, 'L', jliac)
    call jeveuo(apcoef, 'L', japcoe)
    if (llf+llf1+llf2 .ne. 0) then
        call jeveuo(apcofr, 'L', japcof)
    endif
    call jeveuo(typl, 'L', jtypl)
    call jeveuo(stoc//'.SCIB', 'L', jscib)
    call jeveuo(stoc//'.SCBL', 'L', jscbl)
    call jeveuo(stoc//'.SCDE', 'L', jscde)
!
! --- INITIALISATIONS
!
    typef0 = 'F0'
    neq = zi(lmat+2)
    deklag = 0
    nbbloc = zi(jscde-1+3)
    ouvert = '&CFACA2.TRAV'
!
    call wkvect(ouvert, 'V V L', nbbloc, jouv)
    if (ndim .eq. 3) then
        do 100 iliac = 1, spliai
            if (zk8(jtypl-1+iliac) .eq. typef0) then
                deklag = deklag + 1
            endif
100      continue
    endif
! ======================================================================
! --- CALCUL DE -A.C-1.AT (REDUITE AUX LIAISONS ACTIVES) ---------------
! --- (STOCKAGE DE LA MOITIE PAR SYMETRIE) -----------------------------
! ======================================================================
    indfac = min(indfac, spliai+deklag+1)
    do 210 iliac = spliai+1, nbliac + llf + llf1 + llf2
        lliac = zi(jliac-1+iliac)
        call cftyli(resoco, iliac, posit)
        goto (1000, 2000, 4000, 5000) posit
! ======================================================================
! --- AJOUT D'UNE LIAISON DE CONTACT -----------------------------------
! ======================================================================
1000      continue
        call jeveuo(jexnum(cm1a, lliac), 'L', jcm1a)
        ii = zi(jscib-1+iliac+deklag)
        dercol=zi(jscbl+ii-1)
        bloc=dercol*(dercol+1)/2
        if (.not.zl(jouv-1+ii)) then
            if (ii .gt. 1) then
                call jelibe(jexnum(macont//'.UALF', (ii-1)))
                zl(jouv-2+ii)=.false.
            endif
            call jeveuo(jexnum(macont//'.UALF', ii), 'E', jvale)
            zl(jouv-1+ii)=.true.
        endif
        jva = jvale-1 + (iliac+deklag-1)*(iliac+deklag)/2-bloc
        do 10 jj = 1, iliac
            lljac = zi(jliac-1+jj)
            jdecal = zi(japptr+lljac-1)
            nbddl = zi(japptr+lljac) - zi(japptr+lljac-1)
            jva = jva + 1
            zr(jva) = 0.0d0
            call cftyli(resoco, jj, posit)
            goto (1100, 1200, 1300, 1400) posit
! ======================================================================
! --- LIAISON DE CONTACT -----------------------------------------------
! ======================================================================
1100          continue
            call caladu(neq, nbddl, zr(japcoe+jdecal), zi(japddl+ jdecal), zr(jcm1a),&
                        val)
            zr(jva) = zr(jva) - val
            if (abs(zr(jva)) .gt. xjvmax) xjvmax = abs(zr(jva))
            goto 10
! ======================================================================
! --- LIAISON DE FROTTEMENT --------------------------------------------
! ======================================================================
1200          continue
            call caladu(neq, nbddl, zr(japcof+jdecal), zi(japddl+ jdecal), zr(jcm1a),&
                        val)
            zr(jva) = zr(jva) - val
            if (abs(zr(jva)) .gt. xjvmax) xjvmax = abs(zr(jva))
! ======================================================================
! --- DANS LE CAS 3D ---------------------------------------------------
! ======================================================================
            if (ndim .eq. 3) then
                jva = jva + 1
                zr(jva) = 0.0d0
                call caladu(neq, nbddl, zr(japcof+jdecal+30*nesmax), zi(japddl+jdecal),&
                            zr(jcm1a), val)
                zr(jva) = zr(jva) - val
                if (abs(zr(jva)) .gt. xjvmax) xjvmax = abs(zr(jva))
            endif
            goto 10
! ======================================================================
! --- LIAISON DE FROTTEMENT SUIVANT LA PREMIERE DIRECTION --------------
! ======================================================================
1300          continue
            call caladu(neq, nbddl, zr(japcof+jdecal), zi(japddl+ jdecal), zr(jcm1a),&
                        val)
            zr(jva) = zr(jva) - val
            if (abs(zr(jva)) .gt. xjvmax) xjvmax = abs(zr(jva))
            goto 10
! ======================================================================
! --- LIAISON DE FROTTEMENT SUIVANT LA SECONDE DIRECTION ---------------
! ======================================================================
1400          continue
            call caladu(neq, nbddl, zr(japcof+jdecal+30*nesmax), zi(japddl+jdecal), zr(jcm1a),&
                        val)
! ======================================================================
            zr(jva) = zr(jva) - val
            if (abs(zr(jva)) .gt. xjvmax) xjvmax = abs(zr(jva))
10      continue
        call jelibe(jexnum(cm1a, lliac))
        goto 210
! ======================================================================
! --- AJOUT D'UNE LIAISON DE FROTTEMENT --------------------------------
! ======================================================================
2000      continue
        call jeveuo(jexnum(cm1a, lliac+nbliai), 'L', jcm1a)
        ii = zi(jscib-1+iliac+deklag)
        dercol=zi(jscbl+ii-1)
        bloc=dercol*(dercol+1)/2
        if (.not.zl(jouv-1+ii)) then
            if (ii .gt. 1) then
                call jelibe(jexnum(macont//'.UALF', (ii-1)))
                zl(jouv-2+ii)=.false.
            endif
            call jeveuo(jexnum(macont//'.UALF', ii), 'E', jvale)
            zl(jouv-1+ii)=.true.
        endif
        jva = jvale-1 + (iliac+deklag-1)*(iliac+deklag)/2-bloc
        do 20 jj = 1, iliac - 1
            lljac = zi(jliac-1+jj)
            jdecal = zi(japptr+lljac-1)
            nbddl = zi(japptr+lljac) - zi(japptr+lljac-1)
            jva = jva + 1
            zr(jva) = 0.0d0
            call cftyli(resoco, jj, posit)
            goto (2100, 2200, 2300, 2400) posit
! ======================================================================
! --- LIAISON DE CONTACT -----------------------------------------------
! ======================================================================
2100          continue
            call caladu(neq, nbddl, zr(japcoe+jdecal), zi(japddl+ jdecal), zr(jcm1a),&
                        val)
            zr(jva) = zr(jva) - val
            if (abs(zr(jva)) .gt. xjvmax) xjvmax = abs(zr(jva))
            goto 20
! ======================================================================
! --- LIAISON DE FROTTEMENT --------------------------------------------
! ======================================================================
2200          continue
            call caladu(neq, nbddl, zr(japcof+jdecal), zi(japddl+ jdecal), zr(jcm1a),&
                        val)
            zr(jva) = zr(jva) - val
            if (abs(zr(jva)) .gt. xjvmax) xjvmax = abs(zr(jva))
! ======================================================================
! --- DANS LE CAS 3D ---------------------------------------------------
! ======================================================================
            if (ndim .eq. 3) then
                jva = jva + 1
                zr(jva) = 0.0d0
                call caladu(neq, nbddl, zr(japcof+jdecal+30*nesmax), zi(japddl+jdecal),&
                            zr(jcm1a), val)
                zr(jva) = zr(jva) - val
                if (abs(zr(jva)) .gt. xjvmax) xjvmax = abs(zr(jva))
            endif
            goto 20
! ======================================================================
! --- LIAISON DE FROTTEMENT SUIVANT LA PREMIERE DIRECTION --------------
! ======================================================================
2300          continue
            call caladu(neq, nbddl, zr(japcof+jdecal), zi(japddl+ jdecal), zr(jcm1a),&
                        val)
            zr(jva) = zr(jva) - val
            if (abs(zr(jva)) .gt. xjvmax) xjvmax = abs(zr(jva))
            goto 20
! ======================================================================
! --- LIAISON DE FROTTEMENT SUIVANT LA SECONDE DIRECTION ---------------
! ======================================================================
2400          continue
            call caladu(neq, nbddl, zr(japcof+jdecal+30*nesmax), zi(japddl+jdecal), zr(jcm1a),&
                        val)
! ======================================================================
            zr(jva) = zr(jva) - val
            if (abs(zr(jva)) .gt. xjvmax) xjvmax = abs(zr(jva))
20      continue
        lljac = zi(jliac-1+iliac)
        jdecal = zi(japptr+lljac-1)
        nbddl = zi(japptr+lljac) - zi(japptr+lljac-1)
        jva = jva + 1
        zr(jva) = 0.0d0
        call caladu(neq, nbddl, zr(japcof+jdecal), zi(japddl+jdecal), zr(jcm1a),&
                    val)
        zr(jva) = zr(jva) - val
        if (abs(zr(jva)) .gt. xjvmax) xjvmax = abs(zr(jva))
        call jelibe(jexnum(cm1a, lliac+nbliai))
        if (ndim .eq. 3) then
! ======================================================================
! --- DANS LE CAS 3D ---------------------------------------------------
! ======================================================================
            call jeveuo(jexnum(cm1a, lliac+(ndim-1)*nbliai), 'L', jcm1a)
            deklag = deklag + 1
            ii = zi(jscib-1+iliac+deklag)
            dercol=zi(jscbl+ii-1)
            bloc=dercol*(dercol+1)/2
            if (.not.zl(jouv-1+ii)) then
                if (ii .gt. 1) then
                    call jelibe(jexnum(macont//'.UALF', (ii-1)))
                    zl(jouv-2+ii)=.false.
                endif
                call jeveuo(jexnum(macont//'.UALF', ii), 'E', jvale)
                zl(jouv-1+ii)=.true.
            endif
            jva = jvale-1 + (iliac+deklag-1)*(iliac+deklag)/2-bloc
            do 30 jj = 1, iliac
                lljac = zi(jliac-1+jj)
                jdecal = zi(japptr+lljac-1)
                nbddl = zi(japptr+lljac) - zi(japptr+lljac-1)
                jva = jva + 1
                zr(jva) = 0.0d0
                call cftyli(resoco, jj, posit)
                goto (3100, 3200, 3300, 3400) posit
! ======================================================================
! --- LIAISON DE CONTACT -----------------------------------------------
! ======================================================================
3100              continue
                call caladu(neq, nbddl, zr(japcoe+jdecal), zi(japddl+ jdecal), zr(jcm1a),&
                            val)
                zr(jva) = zr(jva) - val
                if (abs(zr(jva)) .gt. xjvmax) xjvmax = abs(zr(jva))
                goto 30
! ======================================================================
! --- LIAISON DE FROTTEMENT --------------------------------------------
! ======================================================================
3200              continue
                call caladu(neq, nbddl, zr(japcof+jdecal), zi(japddl+ jdecal), zr(jcm1a),&
                            val)
                zr(jva) = zr(jva) - val
                if (abs(zr(jva)) .gt. xjvmax) xjvmax = abs(zr(jva))
! ======================================================================
! --- DANS LE CAS 3D ---------------------------------------------------
! ======================================================================
                jva = jva + 1
                zr(jva) = 0.0d0
                call caladu(neq, nbddl, zr(japcof+jdecal+30*nesmax), zi(japddl+jdecal),&
                            zr(jcm1a), val)
                zr(jva) = zr(jva) - val
                if (abs(zr(jva)) .gt. xjvmax) xjvmax = abs(zr(jva))
                goto 30
! ======================================================================
! --- LIAISON DE FROTTEMENT SUIVANT LA PREMIERE DIRECTION --------------
! ======================================================================
3300              continue
                call caladu(neq, nbddl, zr(japcof+jdecal), zi(japddl+ jdecal), zr(jcm1a),&
                            val)
                zr(jva) = zr(jva) - val
                if (abs(zr(jva)) .gt. xjvmax) xjvmax = abs(zr(jva))
                goto 30
! ======================================================================
! --- LIAISON DE FROTTEMENT SUIVANT LA SECONDE DIRECTION ---------------
! ======================================================================
3400              continue
                call caladu(neq, nbddl, zr(japcof+jdecal+30*nesmax), zi(japddl+jdecal),&
                            zr(jcm1a), val)
! ======================================================================
                zr(jva) = zr(jva) - val
                if (abs(zr(jva)) .gt. xjvmax) xjvmax = abs(zr(jva))
30          continue
!            DEKLAG = DEKLAG + 1
            call jelibe(jexnum(cm1a, lliac+(ndim-1)*nbliai))
        endif
        goto 210
! ======================================================================
! --- AJOUT D'UNE LIAISON DE FROTTEMENT SUIVANT LA PREMIERE ------------
! --- DIRECTION UNIQUEMENT ---------------------------------------------
! ======================================================================
4000      continue
        call jeveuo(jexnum(cm1a, lliac+nbliai), 'L', jcm1a)
        ii = zi(jscib-1+iliac+deklag)
        dercol=zi(jscbl+ii-1)
        bloc=dercol*(dercol+1)/2
        if (.not.zl(jouv-1+ii)) then
            if (ii .gt. 1) then
                call jelibe(jexnum(macont//'.UALF', (ii-1)))
                zl(jouv-2+ii)=.false.
            endif
            call jeveuo(jexnum(macont//'.UALF', ii), 'E', jvale)
            zl(jouv-1+ii)=.true.
        endif
        jva = jvale-1 + (iliac+deklag-1)*(iliac+deklag)/2-bloc
        do 40 jj = 1, iliac
            lljac = zi(jliac-1+jj)
            jdecal = zi(japptr+lljac-1)
            nbddl = zi(japptr+lljac) - zi(japptr+lljac-1)
            jva = jva + 1
            zr(jva) = 0.0d0
            call cftyli(resoco, jj, posit)
            goto (4100, 4200, 4300, 4400) posit
! ======================================================================
! --- LIAISON DE CONTACT -----------------------------------------------
! ======================================================================
4100          continue
            call caladu(neq, nbddl, zr(japcoe+jdecal), zi(japddl+ jdecal), zr(jcm1a),&
                        val)
            zr(jva) = zr(jva) - val
            if (abs(zr(jva)) .gt. xjvmax) xjvmax = abs(zr(jva))
            goto 40
! ======================================================================
! --- LIAISON DE FROTTEMENT --------------------------------------------
! ======================================================================
4200          continue
            call caladu(neq, nbddl, zr(japcof+jdecal), zi(japddl+ jdecal), zr(jcm1a),&
                        val)
            zr(jva) = zr(jva) - val
            if (abs(zr(jva)) .gt. xjvmax) xjvmax = abs(zr(jva))
! ======================================================================
! --- DANS LE CAS 3D ---------------------------------------------------
! ======================================================================
            if (ndim .eq. 3) then
                jva = jva + 1
                zr(jva) = 0.0d0
                call caladu(neq, nbddl, zr(japcof+jdecal+30*nesmax), zi(japddl+jdecal),&
                            zr(jcm1a), val)
                zr(jva) = zr(jva) - val
                if (abs(zr(jva)) .gt. xjvmax) xjvmax = abs(zr(jva))
            endif
            goto 40
! ======================================================================
! --- LIAISON DE FROTTEMENT SUIVANT LA PREMIERE DIRECTION --------------
! ======================================================================
4300          continue
            call caladu(neq, nbddl, zr(japcof+jdecal), zi(japddl+ jdecal), zr(jcm1a),&
                        val)
            zr(jva) = zr(jva) - val
            if (abs(zr(jva)) .gt. xjvmax) xjvmax = abs(zr(jva))
            goto 40
! ======================================================================
! --- LIAISON DE FROTTEMENT SUIVANT LA SECONDE DIRECTION ---------------
! ======================================================================
4400          continue
            call caladu(neq, nbddl, zr(japcof+jdecal+30*nesmax), zi(japddl+jdecal), zr(jcm1a),&
                        val)
! ======================================================================
            zr(jva) = zr(jva) - val
            if (abs(zr(jva)) .gt. xjvmax) xjvmax = abs(zr(jva))
40      continue
        call jelibe(jexnum(cm1a, lliac+nbliai))
        goto 210
! ======================================================================
! --- AJOUT D'UNE LIAISON DE CONTACT -----------------------------------
! ======================================================================
5000      continue
        call jeveuo(jexnum(cm1a, lliac+(ndim-1)*nbliai), 'L', jcm1a)
        ii = zi(jscib-1+iliac+deklag)
        dercol=zi(jscbl+ii-1)
        bloc=dercol*(dercol+1)/2
        if (.not.zl(jouv-1+ii)) then
            if (ii .gt. 1) then
                call jelibe(jexnum(macont//'.UALF', (ii-1)))
                zl(jouv-2+ii)=.false.
            endif
            call jeveuo(jexnum(macont//'.UALF', ii), 'E', jvale)
            zl(jouv-1+ii)=.true.
        endif
        jva = jvale-1 + (iliac+deklag-1)*(iliac+deklag)/2-bloc
        do 50 jj = 1, iliac
            lljac = zi(jliac-1+jj)
            jdecal = zi(japptr+lljac-1)
            nbddl = zi(japptr+lljac) - zi(japptr+lljac-1)
            jva = jva + 1
            zr(jva) = 0.0d0
            call cftyli(resoco, jj, posit)
            goto (5100, 5200, 5300, 5400) posit
! ======================================================================
! --- LIAISON DE CONTACT -----------------------------------------------
! ======================================================================
5100          continue
            call caladu(neq, nbddl, zr(japcoe+jdecal), zi(japddl+ jdecal), zr(jcm1a),&
                        val)
            zr(jva) = zr(jva) - val
            if (abs(zr(jva)) .gt. xjvmax) xjvmax = abs(zr(jva))
            goto 50
! ======================================================================
! --- LIAISON DE FROTTEMENT --------------------------------------------
! ======================================================================
5200          continue
            call caladu(neq, nbddl, zr(japcof+jdecal), zi(japddl+ jdecal), zr(jcm1a),&
                        val)
            zr(jva) = zr(jva) - val
            if (abs(zr(jva)) .gt. xjvmax) xjvmax = abs(zr(jva))
! ======================================================================
! --- DANS LE CAS 3D ---------------------------------------------------
! ======================================================================
            if (ndim .eq. 3) then
                jva = jva + 1
                zr(jva) = 0.0d0
                call caladu(neq, nbddl, zr(japcof+jdecal+30*nesmax), zi(japddl+jdecal),&
                            zr(jcm1a), val)
                zr(jva) = zr(jva) - val
                if (abs(zr(jva)) .gt. xjvmax) xjvmax = abs(zr(jva))
            endif
            goto 50
! ======================================================================
! --- LIAISON DE FROTTEMENT SUIVANT LA PREMIERE DIRECTION --------------
! ======================================================================
5300          continue
            call caladu(neq, nbddl, zr(japcof+jdecal), zi(japddl+ jdecal), zr(jcm1a),&
                        val)
            zr(jva) = zr(jva) - val
            if (abs(zr(jva)) .gt. xjvmax) xjvmax = abs(zr(jva))
            goto 50
! ======================================================================
! --- LIAISON DE FROTTEMENT SUIVANT LA SECONDE DIRECTION ---------------
! ======================================================================
5400          continue
            call caladu(neq, nbddl, zr(japcof+jdecal+30*nesmax), zi(japddl+jdecal), zr(jcm1a),&
                        val)
! ======================================================================
            zr(jva) = zr(jva) - val
            if (abs(zr(jva)) .gt. xjvmax) xjvmax = abs(zr(jva))
50      continue
        call jelibe(jexnum(cm1a, lliac+(ndim-1)*nbliai))
210  end do
!
! ======================================================================
    spliai = nbliac + llf + llf1 + llf2
    call jedetr(ouvert)
    call jedema()
! ======================================================================
!
!     -- ON CREE L'OBJET .VALM DE LA MATRICE "MORSE" :
!     CALL UALFVA(MACONT,'V')
!
!
end subroutine

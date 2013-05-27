subroutine elpiv2(xjvmax, ndim, indic, nbliac, ajliai,&
                  spliai, llf, llf1, llf2, noma,&
                  defico, resoco)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    implicit     none
    include 'jeveux.h'
!
    include 'asterfort/cfdisd.h'
    include 'asterfort/cfimp2.h'
    include 'asterfort/cftabl.h'
    include 'asterfort/cftyli.h'
    include 'asterfort/infniv.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jelibe.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/wkvect.h'
    integer :: ndim
    integer :: indic
    integer :: nbliac, llf, llf1, llf2
    integer :: ajliai, spliai
    real(kind=8) :: xjvmax
    character(len=8) :: noma
    character(len=24) :: resoco, defico
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES DISCRETES - UTILITAIRE)
!
! ELIMINATION DES PIVOTS NULS DANS LA MATRICE DE CONTACT/FROTTEMENT
!
! ----------------------------------------------------------------------
!
!
! IN  XJVMAX : VALEUR DU PIVOT MAX
! IN  NDIM   : DIMENSION DU PROBLEME
! OUT INDIC  : +1 ON A RAJOUTE UNE LIAISON
!              -1 ON A ENLEVE UNE LIAISON
! I/O NBLIAC : NOMBRE DE LIAISONS ACTIVES
! I/O AJLIAI : INDICE DANS LA LISTE DES LIAISONS ACTIVES DE LA DERNIERE
!              LIAISON CORRECTE DU CALCUL
!              DE LA MATRICE DE CONTACT ACM1AT
! I/O SPLIAI : INDICE DANS LA LISTE DES LIAISONS ACTIVES DE LA DERNIERE
!              LIAISON AYANT ETE CALCULEE POUR LE VECTEUR CM1A
! IN  NOMA   : NOM DU MAILLAGE
! I/O LLF    : NOMBRE DE LIAISONS DE FROTTEMENT (EN 2D)
!              NOMBRE DE LIAISONS DE FROTTEMENT SUIVANT LES DEUX
!               DIRECTIONS SIMULTANEES (EN 3D)
! I/O LLF1   : NOMBRE DE LIAISONS DE FROTTEMENT SUIVANT LA
!               PREMIERE DIRECTION (EN 3D)
! I/O LLF2   : NOMBRE DE LIAISONS DE FROTTEMENT SUIVANT LA
!               SECONDE DIRECTION (EN 3D)
! IN  DEFICO : SD DE DEFINITION DU CONTACT (ISSUE D'AFFE_CHAR_MECA)
! IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
!                'E': RESOCO(1:14)//'.LIAC'
!                'E': RESOCO(1:14)//'.LIOT'
!
!
!
!
    integer :: kk1, kk2, jva, niv, iliac, ifm
    integer :: note2, note1, note12, note, nbliai, llf0
    integer :: pivot2, lliac, ibid, posit
    integer :: btotal, deklag, pivot, jscde, nbbloc, jouv
    real(kind=8) :: copmax
    character(len=1) :: typeaj, typesp
    character(len=2) :: typec0, typef0, typef1, typef2
    character(len=19) :: liac, liot, macont, stoc, ouvert
    integer :: jliac, jliot, jvale, iscib, ii, dercol, bloc, jscbl
!
! ----------------------------------------------------------------------
!
    call infniv(ifm, niv)
    call jemarq()
!
! --- LECTURE DES STRUCTURES DE DONNEES
!
    liac = resoco(1:14)//'.LIAC'
    liot = resoco(1:14)//'.LIOT'
    macont = resoco(1:14)//'.MATC'
    stoc = resoco(1:14)//'.SLCS'
    call jeveuo(liac, 'E', jliac)
    call jeveuo(liot, 'E', jliot)
    call jeveuo(stoc//'.SCIB', 'L', iscib)
    call jeveuo(stoc//'.SCBL', 'L', jscbl)
    call jeveuo(stoc//'.SCDE', 'L', jscde)
!
! --- INITIALISATIONS
!
    nbbloc = zi(jscde-1+3)
    nbliai = cfdisd(resoco,'NBLIAI')
    ibid = 0
    pivot = 0
    deklag = 0
    typeaj = 'A'
    typesp = 'S'
    typec0 = 'C0'
    typef0 = 'F0'
    typef1 = 'F1'
    typef2 = 'F2'
    ouvert='&&ELPIV2.TRAV'
    call wkvect(ouvert, 'V V L', nbbloc, jouv)
    copmax = xjvmax * 1.0d-08
    if (ndim .eq. 3) then
        llf0 = llf
    else
        llf0 = 0
    endif
! ======================================================================
! --- VERIFICATION DE LA PRESENCE OU NON D'UN PIVOT NUL
! --- SUR L'ENSEMBLE DES LIAISONS EN CONTACT ET ADHERENTES
! ======================================================================
    btotal = nbliac + llf + llf1 + llf2 - 1
    do 90 kk1 = 0, btotal
        iliac = btotal + 1 - kk1
        lliac = zi(jliac-1+iliac)
        call cftyli(resoco, iliac, posit)
        goto (1000, 2000, 3000, 4000) posit
1000      continue
! ======================================================================
! --- ON SE TROUVE DANS LE CAS D'UNE LIAISON DE CONTACT
! ======================================================================
        ii = zi(iscib-1+iliac+llf0-deklag)
        dercol=zi(jscbl+ii-1)
        bloc=dercol*(dercol+1)/2
        if (.not.zl(jouv-1+ii)) then
            if ((ii.lt.nbbloc) .and. (iliac.ne.(btotal+1))) then
                call jelibe(jexnum(macont//'.UALF', (ii+1)))
                zl(jouv+ii)=.false.
            endif
            call jeveuo(jexnum(macont//'.UALF', ii), 'E', jvale)
            zl(jouv-1+ii)=.true.
        endif
        jva=jvale-1+(iliac+llf0-deklag-1)*(iliac+llf0-deklag)/2-bloc
        do 10 kk2 = 1, iliac + llf0 - deklag
            jva = jva + 1
            if (abs(zr(jva)) .lt. copmax) then
                pivot = 1
            else
! ======================================================================
! --- PAS DE PIVOT NUL A OTER, ON PASSE A LA LIAISON SUIVANTE
! ======================================================================
                pivot = 0
                goto 90
            endif
10      continue
! ======================================================================
! --- ON INCREMENTE LE VECTEUR DES LIAISONS OTEES LIOT
! ======================================================================
        zi(jliot+4*nbliai) = zi(jliot+4*nbliai) + 1
        note = zi(jliot+4*nbliai)
        zi(jliot-1+note) = lliac
! ======================================================================
! --- MISE A JOUR DU VECTEUR DES LIAISONS DE CONTACT
! ======================================================================
        call cftabl(indic, nbliac, ajliai, spliai, llf,&
                    llf1, llf2, resoco, typesp, iliac,&
                    lliac, typec0)
        call cfimp2(defico, resoco, noma, lliac, typec0,&
                    'PIV')
        goto 100
2000      continue
! ======================================================================
! --- ON SE TROUVE DANS LE CAS D'UNE LIAISON DE FROTTEMENT ADHERENT
! --- (DANS LE CAS GENERAL EN 2D/SUIVANT LES DEUX DIRECTIONS EN 3D)
! ======================================================================
        if (ndim .eq. 3) then
            deklag = deklag + 1
            ii = zi(iscib-1+iliac+llf0-deklag)
            dercol=zi(jscbl+ii-1)
            bloc=dercol*(dercol+1)/2
            if (.not.zl(jouv-1+ii)) then
                if ((ii.lt.nbbloc) .and. (iliac.ne.(btotal+1))) then
                    call jelibe(jexnum(macont//'.UALF', (ii+1)))
                    zl(jouv+ii)=.false.
                endif
                call jeveuo(jexnum(macont//'.UALF', ii), 'E', jvale)
                zl(jouv-1+ii)=.true.
            endif
            jva=jvale-1+(iliac+llf0-deklag-1)*(iliac+llf0-deklag)/2-&
            bloc
            do 20 kk2 = 1, iliac + llf0 - deklag
                jva = jva + 1
                if (abs(zr(jva)) .lt. copmax) then
                    pivot = 1
                else
! ======================================================================
! --- PAS DE PIVOT NUL A OTER, ON PASSE A LA LIAISON SUIVANTE
! ======================================================================
                    pivot = 0
                endif
20          continue
            do 30 kk2 = 1, iliac + llf0 - deklag + 1
                jva = jva + 1
                if (abs(zr(jva)) .lt. copmax) then
                    pivot2 = 1
                else
! ======================================================================
! --- PAS DE PIVOT NUL A OTER, ON PASSE A LA LIAISON SUIVANTE
! ======================================================================
                    pivot2 = 0
                endif
30          continue
            if (pivot .eq. 0) then
                if (pivot2 .eq. 0) then
! ======================================================================
! --- PAS D'ELIMINATION DE PIVOT
! ======================================================================
                    goto 90
                else
! ======================================================================
! --- ELIMINATION DU PIVOT NUL SUIVANT LA SECONDE DIRECTION
! ======================================================================
                    zi(jliot+4*nbliai+3) = zi(jliot+4*nbliai+3) + 1
                    note2 = zi(jliot+4*nbliai+3)
                    zi(jliot-1+note2+3*nbliai) = lliac
! ======================================================================
! --- MISE A JOUR DU VECTEUR DES LIAISONS DE FROTTEMENT
! --- ON SUPPRIME LA DOUBLE (F0), ON AJOUTE LA F1 -> LA F2 EST SUPPRIMEE
! ======================================================================
                    call cftabl(indic, nbliac, ajliai, spliai, llf,&
                                llf1, llf2, resoco, typesp, iliac,&
                                lliac, typef0)
                    posit = nbliac + llf + llf1 + llf2 + 1
                    call cftabl(ibid, nbliac, ajliai, spliai, llf,&
                                llf1, llf2, resoco, typeaj, posit,&
                                lliac, typef1)
                    call cfimp2(defico, resoco, noma, lliac, typef2,&
                                'PIV')
                    goto 100
                endif
            else
                if (pivot2 .eq. 0) then
! ======================================================================
! --- ELIMINATION DU PIVOT NUL SUIVANT LA SECONDE DIRECTION
! ======================================================================
                    zi(jliot+4*nbliai+2) = zi(jliot+4*nbliai+2) + 1
                    note1 = zi(jliot+4*nbliai+2)
                    zi(jliot-1+note1+2*nbliai) = lliac
! ======================================================================
! --- MISE A JOUR DU VECTEUR DES LIAISONS DE FROTTEMENT ----------------
! ======================================================================
                    call cftabl(indic, nbliac, ajliai, spliai, llf,&
                                llf1, llf2, resoco, typesp, iliac,&
                                lliac, typef0)
                    posit = nbliac + llf + llf1 + llf2 + 1
                    call cftabl(ibid, nbliac, ajliai, spliai, llf,&
                                llf1, llf2, resoco, typeaj, posit,&
                                lliac, typef2)
                    call cfimp2(defico, resoco, noma, lliac, typef1,&
                                'PIV')
                    goto 100
                else
! ======================================================================
! --- ELIMINATION DU PIVOT NUL SUIVANT LES DEUX DIRECTIONS
! ======================================================================
                    zi(jliot+4*nbliai+1) = zi(jliot+4*nbliai+1) + 1
                    note12 = zi(jliot+4*nbliai+1)
                    zi(jliot-1+note12+nbliai) = lliac
! ======================================================================
! --- MISE A JOUR DU VECTEUR DES LIAISONS DE FROTTEMENT
! ======================================================================
                    call cftabl(indic, nbliac, ajliai, spliai, llf,&
                                llf1, llf2, resoco, typesp, iliac,&
                                lliac, typef0)
                    goto 100
                endif
            endif
        else
            ii = zi(iscib-1+iliac+llf0-deklag)
            dercol=zi(jscbl+ii-1)
            bloc=dercol*(dercol+1)/2
            if (.not.zl(jouv-1+ii)) then
                if ((ii.lt.nbbloc) .and. (iliac.ne.(btotal+1))) then
                    call jelibe(jexnum(macont//'.UALF', (ii+1)))
                    zl(jouv+ii)=.false.
                endif
                call jeveuo(jexnum(macont//'.UALF', ii), 'E', jvale)
                zl(jouv-1+ii)=.true.
            endif
            jva=jvale-1+(iliac+llf0-deklag-1)*(iliac+llf0-deklag)/2-&
            bloc
            do 40 kk2 = 1, iliac + llf0 - deklag
                jva = jva + 1
                if (abs(zr(jva)) .lt. copmax) then
                    pivot = 1
                else
! ======================================================================
! --- PAS DE PIVOT NUL A OTER, ON PASSE A LA LIAISON SUIVANTE
! ======================================================================
                    pivot = 0
                    goto 90
                endif
40          continue
            zi(jliot+4*nbliai+1) = zi(jliot+4*nbliai+1) + 1
            note12 = zi(jliot+4*nbliai+1)
            zi(jliot-1+note12+nbliai) = lliac
! ======================================================================
! --- MISE A JOUR DU VECTEUR DES LIAISONS DE FROTTEMENT
! ======================================================================
            call cftabl(indic, nbliac, ajliai, spliai, llf,&
                        llf1, llf2, resoco, typesp, iliac,&
                        lliac, typef0)
            call cfimp2(defico, resoco, noma, lliac, 'F3',&
                        'PIV')
            goto 100
        endif
3000      continue
! ======================================================================
! --- ON SE TROUVE DANS LE CAS D'UNE LIAISON DE FROTTEMENT ADHERENT
! --- SUIVANT LA PREMIERE DIRECTION EN 3D
! ======================================================================
        ii = zi(iscib-1+iliac+llf0-deklag)
        dercol=zi(jscbl+ii-1)
        bloc=dercol*(dercol+1)/2
        if (.not.zl(jouv-1+ii)) then
            if ((ii.lt.nbbloc) .and. (iliac.ne.(btotal+1))) then
                call jelibe(jexnum(macont//'.UALF', (ii+1)))
                zl(jouv+ii)=.false.
            endif
            call jeveuo(jexnum(macont//'.UALF', ii), 'E', jvale)
            zl(jouv-1+ii)=.true.
        endif
        jva=jvale-1+(iliac+llf0-deklag-1)*(iliac+llf0-deklag)/2-bloc
        do 50 kk2 = 1, iliac + llf0 - deklag
            jva = jva + 1
            if (abs(zr(jva)) .lt. copmax) then
                pivot = 1
            else
! ======================================================================
! --- PAS DE PIVOT NUL A OTER, ON PASSE A LA LIAISON SUIVANTE
! ======================================================================
                pivot = 0
                goto 90
            endif
50      continue
        zi(jliot+4*nbliai+1) = zi(jliot+4*nbliai+1) + 1
        note12 = zi(jliot+4*nbliai+1)
        zi(jliot-1+note12+nbliai) = lliac
! ======================================================================
! --- MISE A JOUR DU VECTEUR DES LIAISONS DE FROTTEMENT
! ======================================================================
        call cftabl(indic, nbliac, ajliai, spliai, llf,&
                    llf1, llf2, resoco, typesp, iliac,&
                    lliac, typef1)
        call cfimp2(defico, resoco, noma, lliac, typef1,&
                    'PIV')
        goto 100
4000      continue
! ======================================================================
! --- ON SE TROUVE DANS LE CAS D'UNE LIAISON DE FROTTEMENT ADHERENT
! --- SUIVANT LA SECONDE DIRECTION EN 3D
! ======================================================================
        ii = zi(iscib-1+iliac+llf0-deklag)
        dercol=zi(jscbl+ii-1)
        bloc=dercol*(dercol+1)/2
        if (.not.zl(jouv-1+ii)) then
            if ((ii.lt.nbbloc) .and. (iliac.ne.(btotal+1))) then
                call jelibe(jexnum(macont//'.UALF', (ii+1)))
                zl(jouv+ii)=.false.
            endif
            call jeveuo(jexnum(macont//'.UALF', ii), 'E', jvale)
            zl(jouv-1+ii)=.true.
        endif
        jva=jvale-1+(iliac+llf0-deklag-1)*(iliac+llf0-deklag)/2-bloc
        do 60 kk2 = 1, iliac + llf0 - deklag
            jva = jva + 1
            if (abs(zr(jva)) .lt. copmax) then
                pivot = 1
            else
! ======================================================================
! --- PAS DE PIVOT NUL A OTER, ON PASSE A LA LIAISON SUIVANTE
! ======================================================================
                pivot = 0
                goto 90
            endif
60      continue
        zi(jliot+4*nbliai+1) = zi(jliot+4*nbliai+1) + 1
        note12 = zi(jliot+4*nbliai+1)
        zi(jliot-1+note12+nbliai) = lliac
! ======================================================================
! --- MISE A JOUR DU VECTEUR DES LIAISONS DE FROTTEMENT
! ======================================================================
        call cftabl(indic, nbliac, ajliai, spliai, llf,&
                    llf1, llf2, resoco, typesp, iliac,&
                    lliac, typef2)
        call cfimp2(defico, resoco, noma, lliac, typef2,&
                    'PIV')
        goto 100
! ======================================================================
90  end do
! ======================================================================
100  continue
! ======================================================================
    call jedetr(ouvert)
    call jedema()
! ======================================================================
end subroutine

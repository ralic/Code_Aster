subroutine cfadh3(resoco, defico, noma, ndim, indic,&
                  nbliac, ajliai, spliai, llf, llf1,&
                  llf2)
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
    include 'asterfort/cfimp2.h'
    include 'asterfort/cfmmvd.h'
    include 'asterfort/cftabl.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/wkvect.h'
    character(len=24) :: resoco, defico
    character(len=8) :: noma
    integer :: indic, ndim
    integer :: ajliai, spliai
    integer :: nbliac, llf, llf1, llf2
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES DISCRETES - RESOLUTION)
!
! VERIFICATION QUE LES LIAISONS SONT BIEN ADHERENTES - VERSION 3D
!
! ----------------------------------------------------------------------
!
!
! IN  DEFICO : SD DE DEFINITION DU CONTACT (ISSUE D'AFFE_CHAR_MECA)
! IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
! IN  NOMA   : NOM DU MAILLAGE
! IN  NDIM   : DIMENSION DE L'ESPACE
! OUT INDIC  :+1 ON A RAJOUTE UNE LIAISON
!             -1 ON A ENLEVE UNE LIAISON
! I/O NBLIAC : NOMBRE DE LIAISONS ACTIVES
! I/O AJLIAI : INDICE DANS LA LISTE DES LIAISONS ACTIVES DE LA DERNIERE
!              LIAISON CORRECTE DU CALCUL
!              DE LA MATRICE DE CONTACT ACM1AT
! I/O SPLIAI : INDICE DANS LA LISTE DES LIAISONS ACTIVES DE LA DERNIERE
!              LIAISON AYANT ETE CALCULEE POUR LE VECTEUR CM1A
! I/O LLF    : NOMBRE DE LIAISONS DE FROTTEMENT SUIVANT LES DEUX
!               DIRECTIONS SIMULTANEES (EN 3D)
! I/O LLF1   : NOMBRE DE LIAISONS DE FROTTEMENT SUIVANT LA
!               PREMIERE DIRECTION (EN 3D)
! I/O LLF2   : NOMBRE DE LIAISONS DE FROTTEMENT SUIVANT LA
!               SECONDE DIRECTION (EN 3D)
!
!
!
!
    integer :: btotal
    integer :: iliai, iliac, iliai1, iliai2, iliac1, iliac2
    integer :: icompt
    integer :: comptn, compts, comptu, comptv
    integer :: compt0, compt1, compt2
    character(len=24) :: splf0
    integer :: jsplf0
    real(kind=8) :: coefff, lambdc, xquot
    real(kind=8) :: lambdf, lambf1, lambf2
    character(len=1) :: typesp
    character(len=2) :: typlia, typli1, typli2
    character(len=2) :: typec0, typef0, typef1, typef2
    character(len=19) :: liac, typl, mu
    integer :: jliac, jtypl, jmu
    character(len=24) :: tacfin
    integer :: jtacf
    integer :: ztacf
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    typesp = 'S'
    typec0 = 'C0'
    typef0 = 'F0'
    typef1 = 'F1'
    typef2 = 'F2'
    compt0 = 0
    compt1 = 0
    compt2 = 0
    compts = 0
    comptu = 0
    comptv = 0
    splf0 = '&&CFADH3.SUPLF0'
    btotal = nbliac + llf + llf1 + llf2
!
! --- PAS DE LIAISONS ADHERENTES -> ON SORT
!
    if ((llf+llf1+llf2) .eq. 0) then
        goto 999
    endif
!
! --- LECTURE DES STRUCTURES DE DONNEES DE CONTACT
!
    liac = resoco(1:14)//'.LIAC'
    typl = resoco(1:14)//'.TYPL'
    mu = resoco(1:14)//'.MU'
    tacfin = resoco(1:14)//'.TACFIN'
    call jeveuo(liac, 'L', jliac)
    call jeveuo(typl, 'L', jtypl)
    call jeveuo(mu, 'E', jmu)
    call jeveuo(tacfin, 'L', jtacf)
    ztacf = cfmmvd('ZTACF')
!
! --- VECTEUR POUR STOCKER LES LIAISONS (GLISSANTES) A SUPPRIMER
!
    call wkvect(splf0, 'V V I', llf+llf1+llf2, jsplf0)
!
! --- DETECTION DES LIAISONS EVENTUELLEMENT GLISSANTES
!
    do 300 iliac1 = 1, btotal
!
! ----- PARAMETRES DE LA LIAISON
!
        iliai1 = zi(jliac+iliac1-1)
        typli1 = zk8(jtypl+iliac1-1)(1:2)
!
        if (typli1 .eq. typef0) then
!
! ------- SUIVANT LES DEUX DIRECTIONS
!
            comptn = 0
            compt0 = compt0 + 1
            do 400 iliac2 = 1, iliac1-1
                iliai2 = zi(jliac+iliac2-1)
                typli2 = zk8(jtypl+iliac2-1)(1:2)
                coefff = zr(jtacf+ztacf*(iliai2-1)+1-1)
                if (typli2 .eq. typec0) then
                    comptn = comptn + 1
                endif
                if (iliai1 .eq. iliai2) then
!
! ----------- LAGRANGES DE CONTACT/FROTTEMENT
!
                    lambdc = zr(jmu+comptn-1)
                    lambf1 = zr(jmu+nbliac+ compt0-1)
                    lambf2 = zr(jmu+nbliac+llf+compt0-1)
                    lambdf = sqrt(lambf1**2 + lambf2**2)
!
                    if (lambdc .gt. 0.d0) then
                        xquot = lambdf/lambdc
                    else
                        xquot = 0.d0
                    endif
!
                    if (abs(xquot) .ge. coefff) then
                        compts = compts + 1
                        zi(jsplf0+compts+comptu+comptv-1) = iliac1
                    else
                        zr(jmu+nbliac+compt0-compts-1) = lambf1
                        zr(jmu+nbliac+llf+compt0-compts-1) = lambf2
                    endif
                    goto 300
                endif
400          continue
        else if (typli1.eq.typef1) then
!
! ------- SUIVANT LA PREMIERE DIRECTION
!
            comptn = 0
            compt1 = compt1 + 1
            do 401 iliac2 = 1, iliac1-1
                iliai2 = zi(jliac+iliac2-1)
                typli2 = zk8(jtypl+iliac2-1)(1:2)
                coefff = zr(jtacf+ztacf*(iliai2-1)+1-1)
                if (typli2 .eq. typec0) then
                    comptn = comptn + 1
                endif
                if (iliai1 .eq. iliai2) then
!
! ----------- LAGRANGES DE CONTACT/FROTTEMENT
!
                    lambdc = zr(jmu+comptn-1)
                    lambf1 = zr(jmu+nbliac+(ndim-1)*llf+compt1-1)
!
                    if (lambdc .gt. 0.d0) then
                        xquot = abs(lambf1)/lambdc
                    else
                        xquot = 0.d0
                    endif
!
                    if (abs(xquot) .ge. coefff) then
                        comptu = comptu + 1
                        zi(jsplf0+compts+comptu+comptv-1) = iliac1
                    else
                        zr(jmu+nbliac+(ndim-1)*llf+compt1-comptu-1) =&
                        lambf1
                    endif
                    goto 300
                endif
401          continue
        else if (typli1.eq.typef2) then
!
! ------- SUIVANT LA SECONDE DIRECTION
!
            comptn = 0
            compt2 = compt2 + 1
            do 402 iliac2 = 1, iliac1-1
                iliai2 = zi(jliac+iliac2-1)
                typli2 = zk8(jtypl+iliac2-1)(1:2)
                coefff = zr(jtacf+ztacf*(iliai2-1)+1-1)
                if (typli2 .eq. typec0) then
                    comptn = comptn + 1
                endif
                if (iliai1 .eq. iliai2) then
!
! ----------- LAGRANGES DE CONTACT/FROTTEMENT
!
                    lambdc = zr(jmu+comptn-1)
                    lambf2 = zr(jmu+nbliac+(ndim-1)*llf+llf1+compt2-1)
!
                    if (lambdc .gt. 0.d0) then
                        xquot = abs(lambf2)/lambdc
                    else
                        xquot = 0.d0
                    endif
!
                    if (abs(xquot) .ge. coefff) then
                        comptv = comptv + 1
                        zi(jsplf0+compts+comptu+comptv-1) = iliac1
                    else
                        zr(jmu+nbliac+(ndim-1)*llf+llf1+compt2-comptv-&
                        1) = lambf2
                    endif
                    goto 300
                endif
402          continue
        endif
300  end do
!
! --- SUPPRESSION DES LIAISONS QUI SONT EN FAIT GLISSANTES
!
    do 160 icompt = 1, compts+comptu+comptv
        iliac = zi(jsplf0+compts+comptu+comptv-icompt)
        iliai = zi(jliac+iliac-1)
        typlia = zk8(jtypl+iliac-1)(1:2)
        call cftabl(indic, nbliac, ajliai, spliai, llf,&
                    llf1, llf2, resoco, typesp, iliac,&
                    iliai, typlia)
        call cfimp2(defico, resoco, noma, iliai, typlia,&
                    'GLI')
!
160  end do
!
! --- DECALAGES DES LAGRANGES
!
    if ((compts+comptu+comptv) .ne. 0) then
        do 70 iliac = 1, llf
            zr(jmu+nbliac+llf+iliac-1) = zr(jmu+nbliac+llf+compts+ iliac-1)
70      continue
        do 80 iliac = 1, llf1
            zr(jmu+nbliac+(ndim-1)*llf+iliac-1) = zr( jmu+nbliac+(ndim- 1)*(llf+compts)+iliac-1 )
80      continue
        do 90 iliac = 1, llf2
            zr(jmu+nbliac+(ndim-1)*llf+llf1+iliac-1) = zr(&
                                                       jmu+nbliac+( ndim-1)*(llf+compts)+llf1+com&
                                                       &ptu+iliac-1&
                                                       )
90      continue
    endif
!
    call jedetr(splf0)
!
999  continue
    call jedema()
!
end subroutine

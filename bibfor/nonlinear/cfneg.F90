subroutine cfneg(resoco, defico, noma, ndim, indic,&
                 nbliai, nbliac, ajliai, spliai, llf,&
                 llf1, llf2, nbpren)
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
    implicit     none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/cfimp2.h"
#include "asterfort/cftabl.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/wkvect.h"
    character(len=24) :: resoco, defico
    character(len=8) :: noma
    integer :: ndim
    integer :: indic
    integer :: ajliai, spliai, nbliai, nbpren
    integer :: nbliac, llf, llf1, llf2
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES DISCRETES - RESOLUTION)
!
! VERIFICATION QUE LES MULTIPLICATEURS DE LAGRANGE SONT A VALEURS
!   POSITIVES (PRESSION DE CONTACT POSITIVE)
!
! ----------------------------------------------------------------------
!
!
! IN  DEFICO : SD DE DEFINITION DU CONTACT (ISSUE D'AFFE_CHAR_MECA)
! IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
!                'E': RESOCO(1:14)//'.LIAC'
!                'E': RESOCO(1:14)//'.TYPL'
!                'E': RESOCO(1:14)//'.MU'
! IN  NOMA   : NOM DU MAILLAGE
! IN  NDIM   : DIMENSION DU PROBLEME
! OUT INDIC  :+1 ON A RAJOUTE UNE LIAISON
!             -1 ON A ENLEVE UNE LIAISON
! IN  NBLIAI : NOMBRE DE LIAISONS DE CONTACT
! I/O NBLIAC : NOMBRE DE LIAISONS ACTIVES
! I/O AJLIAI : INDICE DANS LA LISTE DES LIAISONS ACTIVES DE LA DERNIERE
!              LIAISON CORRECTE DU CALCUL
!              DE LA MATRICE DE CONTACT ACM1AT
! I/O SPLIAI : INDICE DANS LA LISTE DES LIAISONS ACTIVES DE LA DERNIERE
!              LIAISON AYANT ETE CALCULEE POUR LE VECTEUR CM1A
! I/O LLF    : NOMBRE DE LIAISONS DE FROTTEMENT (EN 2D)
!              NOMBRE DE LIAISONS DE FROTTEMENT SUIVANT LES DEUX
!               DIRECTIONS SIMULTANEES (EN 3D)
! I/O LLF1   : NOMBRE DE LIAISONS DE FROTTEMENT SUIVANT LA
!               PREMIERE DIRECTION (EN 3D)
! I/O LLF2   : NOMBRE DE LIAISONS DE FROTTEMENT SUIVANT LA
!               SECONDE DIRECTION (EN 3D)
! OUT NBPREN : NOMBRE DE PRESSION NEGATIVE
!
!
!
!
    integer :: deklag, dekln, deklf0, deklf1
    integer :: deklf2, posit, jspnbl, jsplf0, jsplf1, jsplf2
    integer :: jj, kk, ll, mm, iliac, lliac, lljac, jsplia
    integer :: compt0, compt1, compt2, nbini, idebut, ifin, jsto
    real(kind=8) :: lambda
    character(len=1) :: typesp
    character(len=2) :: typec0, typef0, typef1, typef2
    character(len=19) :: liac, mu, typl
    integer :: jliac, jmu, jtypl
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    deklag = 0
    dekln = 0
    deklf0 = 0
    deklf1 = 0
    deklf2 = 0
    nbpren = 0
    typesp = 'S'
    typec0 = 'C0'
    typef0 = 'F0'
    typef1 = 'F1'
    typef2 = 'F2'
!
! --- PAS DE LIAISON DE CONTACT -> ON SORT
!
    if (nbliac .eq. 0) then
        goto 999
    endif
!
! --- LECTURE DES STRUCTURES DE DONNEES DE CONTACT
!
    liac = resoco(1:14)//'.LIAC'
    typl = resoco(1:14)//'.TYPL'
    mu = resoco(1:14)//'.MU'
!
    call jeveuo(liac, 'E', jliac)
    call jeveuo(typl, 'E', jtypl)
    call jeveuo(mu, 'E', jmu)
!
! --- CREATION DES OBJETS DE TRAVAIL
!
    call wkvect('&&CFNEG.SUPNBL', 'V V I', nbliac, jspnbl)
    call wkvect('&&CFNEG.SPLIAC', 'V V I', nbliac, jsplia)
    if (llf .ne. 0) then
        call wkvect('&&CFNEG.SUPLF0', 'V V I', llf, jsplf0)
    endif
    if (llf1 .ne. 0) then
        call wkvect('&&CFNEG.SUPLF1', 'V V I', llf1, jsplf1)
    endif
    if (llf2 .ne. 0) then
        call wkvect('&&CFNEG.SUPLF2', 'V V I', llf2, jsplf2)
    endif
! ======================================================================
! --- LES VALEURS DU VECTEUR SUPNBL SONT NECESSAIREMENT CROISSANTES
! --- ATTENTION CE N'EST PAS NECESSAIREMENT LE CAS DU VECTEUR SUPLLF
! ======================================================================
    nbini = 1
    do 10 iliac = 1, nbliac
        lambda = zr(jmu-1+iliac)
        if (lambda .lt. 0.0d0) then
            dekln = dekln + 1
            do 20 jj = nbini, nbliac + llf + llf1 + llf2
                if (zk8(jtypl-1+jj) .eq. typec0) then
                    deklag = deklag + 1
                    if (deklag .eq. iliac) then
                        zi(jspnbl-1+dekln) = iliac
                        zi(jsplia-1+dekln) = jj
                        nbini = jj + 1
                        lliac = zi(jliac-1+jj)
                        compt0 = 0
                        compt1 = 0
                        compt2 = 0
                        do 30 kk = 1, nbliac + llf + llf1 + llf2
                            lljac = zi(jliac-1+kk)
                            if (zk8(jtypl-1+kk) .eq. typec0) then
                                goto 30
                            else if (zk8(jtypl-1+kk).eq.typef0) then
                                compt0 = compt0 + 1
                                posit = 2
                            else if (zk8(jtypl-1+kk).eq.typef1) then
                                compt1 = compt1 + 1
                                posit = 3
                            else if (zk8(jtypl-1+kk).eq.typef2) then
                                compt2 = compt2 + 1
                                posit = 4
                            endif
                            if (lljac .eq. lliac) then
                                goto (1000, 2000, 3000, 4000) posit
! ======================================================================
! --- CAS IMPOSSIBLE
! ======================================================================
1000                              continue
                                call assert(.false.)
! ======================================================================
! --- CAS DU FROTTEMENT ADHERENT SUIVANT LES DEUX DIRECTIONS EN 3D
! --- OU CAS GENERAL EN 2D
! ======================================================================
2000                              continue
                                deklf0 = deklf0 + 1
                                zi(jsplf0-1+deklf0) = compt0
                                goto 10
! ======================================================================
! --- CAS DU FROTTEMENT ADHERENT SUIVANT LA PREMIERE DIRECTION
! ======================================================================
3000                              continue
                                do 3100 ll = 0, deklf1-1
                                    if (compt1 .gt. zi(jsplf1-1+deklf1- ll)) then
                                        deklf1 = deklf1 + 1
                                        do 3200 mm = deklf1 - ll, deklf1
                                            zi(jsplf1-1+mm) = zi(jsplf1-1+ mm-1 )
3200                                      continue
                                        zi(jsplf1-1+deklf1-ll-1) =&
                                        compt1
                                        goto 10
                                    endif
3100                              continue
                                deklf1 = deklf1 + 1
                                zi(jsplf1-1+deklf1) = compt1
                                goto 10
! ======================================================================
! --- CAS DU FROTTEMENT ADHERENT SUIVANT LA SECONDE DIRECTION
! ======================================================================
4000                              continue
                                do 4100 ll = 0, deklf2-1
                                    if (compt2 .gt. zi(jsplf2-1+deklf2- ll)) then
                                        deklf2 = deklf2 + 1
                                        do 4200 mm = deklf2 - ll, deklf2
                                            zi(jsplf2-1+mm) = zi(jsplf2-1+ mm-1 )
4200                                      continue
                                        zi(jsplf2-1+deklf2-ll-1) =&
                                        compt2
                                        goto 10
                                    endif
4100                              continue
                                deklf2 = deklf2 + 1
                                zi(jsplf2-1+deklf2) = compt2
                                goto 10
! ======================================================================
                            endif
30                      continue
                        goto 10
                    endif
                endif
20          continue
        endif
10  end do
    if (dekln .eq. 0) then
        goto 999
    endif
! ======================================================================
! --- MISE A JOUR DE MU POUR LE CONTACT ET DU VECTEUR DES LIAISONS
! --- DE CONTACT ET DE FROTTEMENT ADHERENT
! ======================================================================
    jsto = zi(jspnbl) - 1
    do 100 iliac = 1, dekln-1
        idebut = jsto + 1
        ifin = idebut+zi(jspnbl-1+iliac+1)-zi(jspnbl-1+iliac)-1-1
        do 110 jj = idebut, ifin
            jsto = jsto + 1
            zr(jmu-1+jsto) = zr(jmu-1+jj+iliac)
110      continue
100  end do
    idebut = jsto + 1
    ifin = nbliac - dekln
    do 120 jj = idebut, ifin
        jsto = jsto + 1
        zr(jmu-1+jsto) = zr(jmu-1+jj+dekln)
120  end do
    do 111 jj = 1, dekln
        iliac = dekln - jj + 1
        posit = zi(jsplia-1+iliac)
        lliac = zi(jliac -1+posit)
        zr(jmu+3*nbliai-1+lliac) = 0.0d0
        call cftabl(indic, nbliac, ajliai, spliai, llf,&
                    llf1, llf2, resoco, typesp, posit,&
                    lliac, typec0)
        call cfimp2(defico, resoco, noma, lliac, typec0,&
                    'NEG')
111  end do
! ======================================================================
! --- MISE A JOUR DE MU POUR LE FROTTEMENT
! ======================================================================
! --- FROTTEMENT ADHERENT DE TYPE LLF
! ======================================================================
    if (llf .ne. 0) then
        if (deklf0 .ne. 0) then
            do 200 iliac = 1, zi(jsplf0-1+1) - 1
                jsto = jsto + 1
                zr(jmu-1+jsto) = zr(jmu-1+nbliac+dekln+iliac)
200          continue
            do 210 iliac = 1, deklf0 - 1
                idebut = jsto + 1
                ifin = idebut + zi(jsplf0-1+iliac+1) - zi(jsplf0-1+ iliac) - 1 - 1
                do 220 jj = idebut, ifin
                    jsto = jsto + 1
                    zr(jmu-1+jsto) = zr(jmu-1+dekln+jj+iliac)
220              continue
210          continue
        endif
        idebut = jsto + 1
        ifin = nbliac + llf
        do 230 jj = idebut, ifin
            jsto = jsto + 1
            zr(jmu-1+jsto) = zr(jmu-1+dekln+deklf0+jj)
230      continue
! ======================================================================
! --- CAS DE LA SECONDE DIRECTION EN 3D
! ======================================================================
        if (ndim .eq. 3) then
            if (deklf0 .ne. 0) then
                do 240 iliac = 1, zi(jsplf0-1+1) - 1
                    jsto = jsto + 1
                    zr(jmu-1+jsto) = zr(jmu-1+nbliac+dekln+llf+deklf0+ iliac)
240              continue
                do 250 iliac = 1, deklf0 - 1
                    idebut = jsto + 1
                    ifin = idebut + zi(jsplf0-1+iliac+1) - zi(jsplf0- 1+iliac) - 1 - 1
                    do 260 jj = idebut, ifin
                        jsto = jsto + 1
                        zr(jmu-1+jsto) = zr(jmu-1+dekln+deklf0+jj)
260                  continue
250              continue
            endif
            idebut = jsto + 1
            ifin = nbliac + (ndim-1)*llf
            do 270 jj = idebut, ifin
                jsto = jsto + 1
                zr(jmu-1+jsto) = zr(jmu-1+dekln+(ndim-1)*deklf0+jj)
270          continue
        endif
    endif
! ======================================================================
! --- FROTTEMENT ADHERENT DE TYPE LLF1
! ======================================================================
    if (llf1 .ne. 0) then
        if (deklf1 .ne. 0) then
            do 300 iliac = 1, zi(jsplf1-1+1) - 1
                jsto = jsto + 1
                zr(jmu-1+jsto) = zr( jmu-1+nbliac+dekln+(ndim-1)*(llf+ deklf0)+iliac )
300          continue
            do 310 iliac = 1, deklf1 - 1
                idebut = zi(jsplf1-1+iliac )
                ifin = zi(jsplf1-1+iliac+1) - 1
                do 320 jj = idebut, ifin
                    jsto = jsto + 1
                    zr(jmu-1+jsto) = zr(jmu-1+dekln+(ndim-1)*deklf0+ jj)
320              continue
310          continue
        endif
        idebut = jsto + 1
        ifin = nbliac + (ndim-1)*llf + llf1
        do 330 jj = idebut, ifin
            jsto = jsto + 1
            zr(jmu-1+jsto) = zr(jmu-1+dekln+(ndim-1)*deklf0+deklf1+jj)
330      continue
    endif
! ======================================================================
! --- FROTTEMENT ADHERENT DE TYPE LLF2
! ======================================================================
    if (llf2 .ne. 0) then
        if (deklf2 .ne. 0) then
            do 400 iliac = 1, zi(jsplf2-1+1) - 1
                jsto = jsto + 1
                zr(jmu-1+jsto) = zr(jmu-1+nbliac+dekln+(ndim-1)*(llf+ deklf0)+llf1+deklf1+iliac)
400          continue
            do 410 iliac = 1, deklf2 - 1
                idebut = zi(jsplf2-1+iliac )
                ifin = zi(jsplf2-1+iliac+1) - 1
                do 420 jj = idebut, ifin
                    jsto = jsto + 1
                    zr(jmu-1+jsto) = zr(jmu-1+dekln+(ndim-1)*deklf0+ deklf1+jj)
420              continue
410          continue
        endif
        idebut = jsto + 1
        ifin = nbliac + (ndim-1)*llf + llf1 + llf2
        do 430 jj = idebut, ifin
            jsto = jsto + 1
            zr(jmu-1+jsto) = zr(jmu-1+dekln+(ndim-1)*deklf0+deklf1+ deklf2+jj)
430      continue
    endif
! ======================================================================
999  continue
!
    nbpren = dekln
! ======================================================================
! --- MENAGE
! ======================================================================
    call jedetr('&&CFNEG.SUPNBL')
    call jedetr('&&CFNEG.SPLIAC')
    call jedetr('&&CFNEG.SUPLF0')
    call jedetr('&&CFNEG.SUPLF1')
    call jedetr('&&CFNEG.SUPLF2')
! ======================================================================
    call jedema()
! ======================================================================
end subroutine

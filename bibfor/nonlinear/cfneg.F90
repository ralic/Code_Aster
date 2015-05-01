subroutine cfneg(resoco, defico, noma, ndim, indic,&
                 nbliai, nbliac, ajliai, spliai, llf,&
                 llf1, llf2, nbpren)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/cfimp2.h"
#include "asterfort/cftabl.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
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
    integer :: deklf2, posit
    integer :: jj, kk, ll, mm, iliac, lliac, lljac
    integer :: compt0, compt1, compt2, nbini, idebut, ifin, jsto
    real(kind=8) :: lambda
    character(len=1) :: typesp
    character(len=2) :: typec0, typef0, typef1, typef2
    character(len=19) :: liac, mu, typl
    integer :: jliac, jmu, jtypl
    integer, pointer :: spliac(:) => null()
    integer, pointer :: suplf0(:) => null()
    integer, pointer :: suplf1(:) => null()
    integer, pointer :: suplf2(:) => null()
    integer, pointer :: supnbl(:) => null()
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
    AS_ALLOCATE(vi=supnbl, size=nbliac)
    AS_ALLOCATE(vi=spliac, size=nbliac)
    if (llf .ne. 0) then
        AS_ALLOCATE(vi=suplf0, size=llf)
    endif
    if (llf1 .ne. 0) then
        AS_ALLOCATE(vi=suplf1, size=llf1)
    endif
    if (llf2 .ne. 0) then
        AS_ALLOCATE(vi=suplf2, size=llf2)
    endif
! ======================================================================
! --- LES VALEURS DU VECTEUR SUPNBL SONT NECESSAIREMENT CROISSANTES
! --- ATTENTION CE N'EST PAS NECESSAIREMENT LE CAS DU VECTEUR SUPLLF
! ======================================================================
    nbini = 1
    do iliac = 1, nbliac
        lambda = zr(jmu-1+iliac)
        if (lambda .lt. 0.0d0) then
            dekln = dekln + 1
            do jj = nbini, nbliac + llf + llf1 + llf2
                if (zk8(jtypl-1+jj) .eq. typec0) then
                    deklag = deklag + 1
                    if (deklag .eq. iliac) then
                        supnbl(dekln) = iliac
                        spliac(dekln) = jj
                        nbini = jj + 1
                        lliac = zi(jliac-1+jj)
                        compt0 = 0
                        compt1 = 0
                        compt2 = 0
                        do kk = 1, nbliac + llf + llf1 + llf2
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
                                ASSERT(posit > 1 .and. posit <= 4)
                                select case (posit)
! ======================================================================
! --- CAS DU FROTTEMENT ADHERENT SUIVANT LES DEUX DIRECTIONS EN 3D
! --- OU CAS GENERAL EN 2D
! ======================================================================
                                case (2)
                                    deklf0 = deklf0 + 1
                                    suplf0(deklf0) = compt0
! ======================================================================
! --- CAS DU FROTTEMENT ADHERENT SUIVANT LA PREMIERE DIRECTION
! ======================================================================
                                case (3)
                                    do ll = 0, deklf1-1
                                        if (compt1 .gt. suplf1(deklf1- ll)) then
                                            deklf1 = deklf1 + 1
                                            do mm = deklf1 - ll, deklf1
                                                suplf1(mm) = suplf1(mm-1 )
                                            end do
                                            suplf1(deklf1-ll-1) =&
                                        compt1
                                            goto 10
                                        endif
                                    end do
                                    deklf1 = deklf1 + 1
                                    suplf1(deklf1) = compt1
! ======================================================================
! --- CAS DU FROTTEMENT ADHERENT SUIVANT LA SECONDE DIRECTION
! ======================================================================
                                case (4)
                                    do ll = 0, deklf2-1
                                        if (compt2 .gt. suplf2(deklf2- ll)) then
                                            deklf2 = deklf2 + 1
                                            do mm = deklf2 - ll, deklf2
                                                suplf2(mm) = suplf2(mm-1 )
                                            end do
                                            suplf2(deklf2-ll-1) =&
                                        compt2
                                            goto 10
                                        endif
                                    end do
                                    deklf2 = deklf2 + 1
                                    suplf2(deklf2) = compt2
                                end select
! ======================================================================
                            endif
 30                         continue
                        end do
                        goto 10
                    endif
                endif
            end do
        endif
 10     continue
    end do
    if (dekln .eq. 0) then
        goto 999
    endif
! ======================================================================
! --- MISE A JOUR DE MU POUR LE CONTACT ET DU VECTEUR DES LIAISONS
! --- DE CONTACT ET DE FROTTEMENT ADHERENT
! ======================================================================
    jsto = supnbl(1) - 1
    do iliac = 1, dekln-1
        idebut = jsto + 1
        ifin = idebut+supnbl(iliac+1)-supnbl(iliac)-1-1
        do jj = idebut, ifin
            jsto = jsto + 1
            zr(jmu-1+jsto) = zr(jmu-1+jj+iliac)
        end do
    end do
    idebut = jsto + 1
    ifin = nbliac - dekln
    do jj = idebut, ifin
        jsto = jsto + 1
        zr(jmu-1+jsto) = zr(jmu-1+jj+dekln)
    end do
    do jj = 1, dekln
        iliac = dekln - jj + 1
        posit = spliac(iliac)
        lliac = zi(jliac -1+posit)
        zr(jmu+3*nbliai-1+lliac) = 0.0d0
        call cftabl(indic, nbliac, ajliai, spliai, llf,&
                    llf1, llf2, resoco, typesp, posit,&
                    lliac, typec0)
        call cfimp2(defico, resoco, noma, lliac, typec0,&
                    'NEG')
    end do
! ======================================================================
! --- MISE A JOUR DE MU POUR LE FROTTEMENT
! ======================================================================
! --- FROTTEMENT ADHERENT DE TYPE LLF
! ======================================================================
    if (llf .ne. 0) then
        if (deklf0 .ne. 0) then
            do iliac = 1, suplf0(1) - 1
                jsto = jsto + 1
                zr(jmu-1+jsto) = zr(jmu-1+nbliac+dekln+iliac)
            end do
            do iliac = 1, deklf0 - 1
                idebut = jsto + 1
                ifin = idebut + suplf0(iliac+1) - suplf0(iliac) - 1 - 1
                do jj = idebut, ifin
                    jsto = jsto + 1
                    zr(jmu-1+jsto) = zr(jmu-1+dekln+jj+iliac)
                end do
            end do
        endif
        idebut = jsto + 1
        ifin = nbliac + llf
        do jj = idebut, ifin
            jsto = jsto + 1
            zr(jmu-1+jsto) = zr(jmu-1+dekln+deklf0+jj)
        end do
! ======================================================================
! --- CAS DE LA SECONDE DIRECTION EN 3D
! ======================================================================
        if (ndim .eq. 3) then
            if (deklf0 .ne. 0) then
                do iliac = 1, suplf0(1) - 1
                    jsto = jsto + 1
                    zr(jmu-1+jsto) = zr(jmu-1+nbliac+dekln+llf+deklf0+ iliac)
                end do
                do iliac = 1, deklf0 - 1
                    idebut = jsto + 1
                    ifin = idebut + suplf0(iliac+1) - suplf0(iliac) - 1 - 1
                    do jj = idebut, ifin
                        jsto = jsto + 1
                        zr(jmu-1+jsto) = zr(jmu-1+dekln+deklf0+jj)
                    end do
                end do
            endif
            idebut = jsto + 1
            ifin = nbliac + (ndim-1)*llf
            do jj = idebut, ifin
                jsto = jsto + 1
                zr(jmu-1+jsto) = zr(jmu-1+dekln+(ndim-1)*deklf0+jj)
            end do
        endif
    endif
! ======================================================================
! --- FROTTEMENT ADHERENT DE TYPE LLF1
! ======================================================================
    if (llf1 .ne. 0) then
        if (deklf1 .ne. 0) then
            do iliac = 1, suplf1(1) - 1
                jsto = jsto + 1
                zr(jmu-1+jsto) = zr( jmu-1+nbliac+dekln+(ndim-1)*(llf+ deklf0)+iliac )
            end do
            do iliac = 1, deklf1 - 1
                idebut = suplf1(iliac )
                ifin = suplf1(iliac+1) - 1
                do jj = idebut, ifin
                    jsto = jsto + 1
                    zr(jmu-1+jsto) = zr(jmu-1+dekln+(ndim-1)*deklf0+ jj)
                end do
            end do
        endif
        idebut = jsto + 1
        ifin = nbliac + (ndim-1)*llf + llf1
        do jj = idebut, ifin
            jsto = jsto + 1
            zr(jmu-1+jsto) = zr(jmu-1+dekln+(ndim-1)*deklf0+deklf1+jj)
        end do
    endif
! ======================================================================
! --- FROTTEMENT ADHERENT DE TYPE LLF2
! ======================================================================
    if (llf2 .ne. 0) then
        if (deklf2 .ne. 0) then
            do iliac = 1, suplf2(1) - 1
                jsto = jsto + 1
                zr(jmu-1+jsto) = zr(jmu-1+nbliac+dekln+(ndim-1)*(llf+ deklf0)+llf1+deklf1+iliac)
            end do
            do iliac = 1, deklf2 - 1
                idebut = suplf2(iliac )
                ifin = suplf2(iliac+1) - 1
                do jj = idebut, ifin
                    jsto = jsto + 1
                    zr(jmu-1+jsto) = zr(jmu-1+dekln+(ndim-1)*deklf0+ deklf1+jj)
                end do
            end do
        endif
        idebut = jsto + 1
        ifin = nbliac + (ndim-1)*llf + llf1 + llf2
        do jj = idebut, ifin
            jsto = jsto + 1
            zr(jmu-1+jsto) = zr(jmu-1+dekln+(ndim-1)*deklf0+deklf1+ deklf2+jj)
        end do
    endif
! ======================================================================
999 continue
!
    nbpren = dekln
! ======================================================================
! --- MENAGE
! ======================================================================
    AS_DEALLOCATE(vi=supnbl)
    AS_DEALLOCATE(vi=spliac)
    AS_DEALLOCATE(vi=suplf0)
    AS_DEALLOCATE(vi=suplf1)
    AS_DEALLOCATE(vi=suplf2)
! ======================================================================
    call jedema()
! ======================================================================
end subroutine

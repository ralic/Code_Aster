subroutine vppfac(lmasse, masgen, vect, neq, nbvect,&
                  mxvect, masmod, facpar)
    implicit none
#include "jeveux.h"
#include "asterc/r8maem.h"
#include "asterc/r8miem.h"
#include "asterc/r8vide.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jemarq.h"
#include "asterfort/mrmult.h"
#include "asterfort/pteddl.h"
#include "asterfort/wkvect.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rsvpar.h"
#include "blas/ddot.h"
#include "asterc/gettco.h"
!
    integer :: lmasse, neq, nbvect, mxvect
    real(kind=8) :: masgen(*), vect(neq, *)
    real(kind=8) :: masmod(mxvect, *), facpar(mxvect, *)
!     ------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     CALCUL DES PARAMETRES MODAUX :
!            FACTEUR DE PARTICIPATION ET MASSE MODALE UNITAIRE
!     ------------------------------------------------------------------
! IN  LMASSE : IS : DESCRIPTEUR NORMALISE DE LA MATRICE DE MASSE
!     ------------------------------------------------------------------
!     PRESUME L'EXECUTION PREALABLE DE VPPGEN : CALCUL DES PARAMETRES
!     MODAUX.
!     ------------------------------------------------------------------
!
!
    integer :: lddl, laux1, laux2, iddl, ia, ieq, ivect, mxddl, jref, iadpar(1), l1, ibid
    parameter     ( mxddl=6 )
    character(len=8) :: nomddl(mxddl),basemo,k8b
    character(len=14) :: nume
    character(len=16) :: nompar(3), typmas, typbas
    character(len=19) :: masse
    character(len=24) :: posddl, vecau1, vecau2
    real(kind=8) :: rmin, rmax, raux, rval
    logical :: gene
!     ------------------------------------------------------------------
    data nomddl / 'DX      ', 'DY      ', 'DZ      ' ,&
     &              'DRX     ', 'DRY     ', 'DRZ     ' /
    data nompar /'FACT_PARTICI_DX','FACT_PARTICI_DY','FACT_PARTICI_DZ'/
!
!     ------------------------------------------------------------------
    data  posddl/'&&VPPFAC.POSITION.DDL'/
    data  vecau1/'&&VPPFAC.VECTEUR.AUX1'/
    data  vecau2/'&&VPPFAC.VECTEUR.AUX2'/
!     ------------------------------------------------------------------
!     ----------------- CREATION DE VECTEURS DE TRAVAIL ----------------
!     ------------------------------------------------------------------
!
    call wkvect(vecau1, 'V V R', neq, laux1)
    call wkvect(vecau2, 'V V R', neq, laux2)
    call wkvect(posddl, 'V V I', neq*mxddl, lddl)
!
!     ------------------------------------------------------------------
!     ------------------------------------------------------------------
!     ------ RECUPERATION DES POSITIONS DU VECTEUR EXCITATION : --------
!     ---- VECTEUR DE 1 DANS LA DIRECTION iddl DANS LE CAS PHYSIQUE ----
!     --- FACTEURS DE PARTICIPATION DES MODES DANS LE CAS GENERALISE ---
!     ------------------------------------------------------------------
!
    call jemarq()
    masse = zk24(zi(lmasse+1))
    call dismoi('NOM_NUME_DDL', masse, 'MATR_ASSE', repk=nume)
    call gettco(masse, typmas)
!
    rmin=100.d0*r8miem()
    rmax=sqrt(r8maem())
!
! DETERMINATION DU CAS : BASE PHYSIQUE OU BASE GENERALISEE
    gene = .false.
    if (typmas(1:14).eq.'MATR_ASSE_GENE') then
! SI MATR_ASSE_GENE : BASE GENERALISEE
           call jeveuo(nume(1:14)//'.NUME.REFN', 'L', jref)
           basemo = zk24(jref)(1:8)
! SAUF SI NUME.REFN POINTE VERS UN MODELE_GENE ET NON VERS UNE BASE
! ALORS CAS DE LA SSD, TRAITE COMME UN MODE_MECA CLASSIQUE
           call gettco(basemo, typbas)
           if (typbas(1:14).eq.'MODE_MECA') then
              gene= .true.
           endif
    endif
    do iddl = 1, 3
        if (gene) then
            do ieq = 1, neq
               call rsvpar( basemo, 1, nompar(iddl), ibid, r8vide(), k8b, l1)
               if (l1 .eq. 100) then
                  zr(laux1+ieq-1) = 0.D0
               else
                  call rsadpa (basemo, 'L', 1,nompar(iddl),ieq,1,tjv=iadpar)
                  zr(laux1+ieq-1) = zr(iadpar(1))
               endif
! SECURITE SI ON EST PASSE PAR DES MODES HETERODOXES AVEC FACTEURS DE PARTICIPATIONS HERETIQUES
           end do
        else
           call pteddl('NUME_DDL', nume, mxddl, nomddl, neq, zi(lddl))
           ia = (iddl-1)*neq
           do ieq = 1, neq
               zr(laux1+ieq-1) = zi(lddl+ia+ieq-1)
           end do
        endif
!
!     ------------------------------------------------------------------
!     ----------- CALCUL DE  FREQ * MASSE * UNITAIRE_DIRECTION ---------
!     ------------------------------------------------------------------
        call mrmult('ZERO', lmasse, zr(laux1), zr(laux2), 1, .false.)
        do ivect = 1, nbvect
            rval = ddot(neq,vect(1,ivect),1,zr(laux2),1)
            raux = masgen(ivect)
            if ((abs(raux) .lt. rmin) .or. (abs(rval) .gt. rmax)) then
                masmod(ivect,iddl) = rmax
                facpar(ivect,iddl) = rmax
            else
                raux=rval/raux
                masmod(ivect,iddl) = rval * raux
                facpar(ivect,iddl) = raux
            endif
        end do
    end do
!
!     ------------------------------------------------------------------
!     ----------------- DESTRUCTION DES VECTEURS DE TRAVAIL ------------
!     ------------------------------------------------------------------
!
    call jedetr(posddl)
    call jedetr(vecau1)
    call jedetr(vecau2)
!
    call jedema()
end subroutine

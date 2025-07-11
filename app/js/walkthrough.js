class WalkthroughManager {
  constructor() {
    this.overlay = null;
    this.tooltip = null;
    this.currentTarget = null;
    this.init();
  }

  init() {
    // listen for shiny messages
    const self = this;
    Shiny.addCustomMessageHandler('updateWalkthroughStep', (data) => {
      self.showStep(data);
    });

    Shiny.addCustomMessageHandler('hideWalkthrough', (data) => {
      self.hide(data);
    });

    // remove unwanted tooltips from all elements
    this.removeDefaultTooltips();
  }

  removeDefaultTooltips() {
    this.observer = null;
    // remove title attributes that cause unwanted tooltips
    document.addEventListener('DOMContentLoaded', () => {
      const elementsWithTitle = document.querySelectorAll('[title]');
      elementsWithTitle.forEach((el) => {
        if (
          el.getAttribute('title')
          && el.getAttribute('title').includes('<')
        ) {
          el.removeAttribute('title');
        }
      });
    });

    // monitor for new elements with unwanted tooltips
    this.observer = new MutationObserver((mutations) => {
      mutations.forEach((mutation) => {
        if (mutation.type === 'childList') {
          mutation.addedNodes.forEach((node) => {
            if (node.nodeType === 1) {
              const elementsWithTitle = node.querySelectorAll
                ? node.querySelectorAll('[title]')
                : [];
              elementsWithTitle.forEach((el) => {
                if (
                  el.getAttribute('title')
                  && el.getAttribute('title').includes('<')
                ) {
                  el.removeAttribute('title');
                }
              });
              if (
                node.getAttribute
                && node.getAttribute('title')
                && node.getAttribute('title').includes('<')
              ) {
                node.removeAttribute('title');
              }
            }
          });
        }
      });
    });

    // Add cleanup method to disconnect observer
    this.cleanup = () => {
      if (this.observer) {
        this.observer.disconnect();
        this.observer = null;
      }
    };
    this.observer.observe(document.body, { childList: true, subtree: true });
  }

  showStep(data) {
    this.overlay = document.getElementById('app-walkthrough_overlay');
    this.tooltip = document.getElementById('app-walkthrough_tooltip');

    if (!this.overlay || !this.tooltip) return;

    // update tooltip content
    document.getElementById('app-step_title').textContent = data.title;
    document.getElementById('app-step_content').textContent = data.content;
    document.getElementById('app-step_counter').textContent = `${data.step_num} of ${data.total_steps}`;

    // update button states
    const prevBtn = document.getElementById('app-prev_step');
    const nextBtn = document.getElementById('app-next_step');

    if (prevBtn) {
      prevBtn.style.display = data.show_prev ? 'inline-block' : 'none';
    }

    if (nextBtn) {
      nextBtn.textContent = data.step_num === data.total_steps ? 'Finish' : 'Next';
    }

    // button event listeners
    this.setupButtonListeners();

    // position tooltip
    this.positionTooltip(data.target, data.placement);

    // show overlay
    this.overlay.classList.remove('d-none');

    // highlight target element
    this.highlightTarget(data.target);
  }

  setupButtonListeners() {
    if (!this.tooltip) return;
    // remove existing listeners to prevent duplicates
    const endBtn = document.getElementById('app-end_walkthrough');
    const nextBtn = document.getElementById('app-next_step');
    const prevBtn = document.getElementById('app-prev_step');

    if (endBtn) {
      endBtn.replaceWith(endBtn.cloneNode(true));
      const newEndBtn = document.getElementById('app-end_walkthrough');
      newEndBtn.addEventListener('click', () => {
        Shiny.setInputValue('app-end_walkthrough', Math.random());
      });
    }

    if (nextBtn) {
      nextBtn.replaceWith(nextBtn.cloneNode(true));
      const newNextBtn = document.getElementById('app-next_step');
      newNextBtn.addEventListener('click', () => {
        Shiny.setInputValue('app-next_step', Math.random());
      });
    }

    if (prevBtn) {
      prevBtn.replaceWith(prevBtn.cloneNode(true));
      const newPrevBtn = document.getElementById('app-prev_step');
      newPrevBtn.addEventListener('click', () => {
        Shiny.setInputValue('app-prev_step', Math.random());
      });
    }
  }

  positionTooltip(targetSelector, placement) {
    const target = document.querySelector(targetSelector);
    if (!target || !this.tooltip) return;

    const targetRect = target.getBoundingClientRect();
    const tooltipRect = this.tooltip.getBoundingClientRect();

    let top; let
      left;

    switch (placement) {
      case 'right':
        top = targetRect.top + targetRect.height / 2 - tooltipRect.height / 2;
        left = targetRect.right + 15;
        break;
      case 'left':
        top = targetRect.top + targetRect.height / 2 - tooltipRect.height / 2;
        left = targetRect.left - tooltipRect.width - 15;
        break;
      case 'bottom':
        top = targetRect.bottom + 15;
        left = targetRect.left + targetRect.width / 2 - tooltipRect.width / 2;
        break;
      case 'top':
      default:
        top = targetRect.top - tooltipRect.height - 15;
        left = targetRect.left + targetRect.width / 2 - tooltipRect.width / 2;
        break;
    }

    // ensure tooltip stays within viewport
    top = Math.max(
      10,
      Math.min(top, window.innerHeight - tooltipRect.height - 10),
    );
    left = Math.max(
      10,
      Math.min(left, window.innerWidth - tooltipRect.width - 10),
    );

    this.tooltip.style.top = `${top}px`;
    this.tooltip.style.left = `${left}px`;
  }

  highlightTarget(targetSelector) {
    // remove previous highlight
    if (this.currentTarget) {
      this.currentTarget.classList.remove('walkthrough-highlight');
    }

    // add new highlight
    const target = document.querySelector(targetSelector);
    if (target) {
      target.classList.add('walkthrough-highlight');
      this.currentTarget = target;

      // scroll target into view
      target.scrollIntoView({
        behavior: 'smooth',
        block: 'center',
        inline: 'nearest',
      });
    }
  }

  hide(data) {
    this.overlay = document.getElementById('app-walkthrough_overlay');

    if (this.overlay) {
      this.overlay.classList.add('d-none');
    }

    if (this.currentTarget) {
      this.currentTarget.classList.remove('walkthrough-highlight');
      this.currentTarget = null;
    }

    // eslint-disable-next-line no-param-reassign, no-unused-vars
    data = Math.random();
  }
}

// init when DOM is ready
document.addEventListener('DOMContentLoaded', () => {
  window.walkthroughManager = new WalkthroughManager();
});

export { WalkthroughManager };
